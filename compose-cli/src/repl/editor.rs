use compose_editor::crossterm;
use compose_editor::crossterm::QueueableCommand;
use compose_editor::crossterm::event::{Event, KeyCode, KeyModifiers};
use compose_editor::crossterm::style::{Colorize, Styler};
use compose_editor::crossterm::terminal::{Clear, ClearType};
use compose_editor::editor::Editor;
use compose_editor::editor::keybindings::Keybinding;
use compose_editor::renderer::styles::{Footer, Margin};
use compose_editor::renderer::{RenderData, Renderer};
use parking_lot::Mutex;
use std::cell::RefCell;
use std::io::Write;
use std::sync::Arc;

pub fn print_input(input: &str, line_offset: usize) {
    for (i, line) in input.lines().enumerate() {
        println!(
            "{:^width$}{sep}{line}",
            i + 1 + line_offset,
            width = MARGIN_WIDTH - 3,
            sep = match (line_offset, i) {
                // (1, 0) => "── ",
                (0, 0) => "╭─ ",
                (_, 0) => "├─ ",
                (_, _) => "│  ",
            }
        );
    }
}

const MARGIN_WIDTH: usize = 6;

pub struct EditorGutter {
    pub(crate) line_offset: usize,
}

impl<W: Write> Margin<W> for EditorGutter {
    fn width(&self) -> usize {
        MARGIN_WIDTH
    }

    fn draw(
        &mut self,
        write: &mut W,
        line_idx: usize,
        data: &RenderData,
    ) -> compose_editor::Result<()> {
        let style = if line_idx == data.focus().ln {
            |str: String| str.white().bold()
        } else {
            |str: String| str.white()
        };
        write!(
            write,
            "{:^width$}{sep}",
            style((line_idx + 1 + self.line_offset).to_string()),
            width = MARGIN_WIDTH - 3,
            sep = style(match (line_idx, self.line_offset) {
                (0, 0) => "╭─ ".to_string(),
                (0, _) => "├─ ".to_string(),
                (_, _) => "│  ".to_string(),
            })
        )?;

        Ok(())
    }
}

pub struct EditorFooter<'a> {
    pub(crate) message: &'a str,
}

impl<W: Write> Footer<W> for EditorFooter<'_> {
    fn rows(&self) -> usize {
        1
    }

    fn draw(&mut self, write: &mut W, data: &RenderData) -> compose_editor::Result<()> {
        write!(
            write,
            "{:─<width$}╯ [{}:{}] {message}",
            "─",
            data.focus().ln,
            data.focus().col.min(data.current_line().len()),
            width = MARGIN_WIDTH - 3,
            message = self.message,
        )?;
        write.queue(Clear(ClearType::UntilNewLine))?;
        Ok(())
    }
}

#[derive(Clone, Debug, Default)]
pub struct EditorHistory(Arc<Mutex<Vec<String>>>);

impl EditorHistory {
    pub fn new() -> Self {
        Self(Arc::new(Mutex::new(vec![])))
    }

    pub fn add(&self, s: impl ToString) {
        let mut history = self.0.lock();
        history.push(s.to_string());
    }

    pub fn len(&self) -> usize {
        self.0.lock().len()
    }

    pub fn get(&self, idx: usize) -> Option<String> {
        self.0.lock().get(idx).cloned()
    }

    pub fn pop(&self) -> Option<String> {
        let mut history = self.0.lock();
        history.pop()
    }
}

pub struct EditorReader<'a> {
    pub history: &'a EditorHistory,
    pub history_idx: RefCell<usize>,
}

impl<'a> EditorReader<'a> {
    pub fn new(history: &'a EditorHistory) -> Self {
        Self {
            history,
            history_idx: RefCell::new(history.len()),
        }
    }

    pub fn history_up(&self, get_current_content: impl Fn() -> String) -> Option<String> {
        if self.history.len() == 0 {
            return None;
        }

        let mut idx = self.history_idx.borrow_mut();
        if *idx == self.history.len() {
            // Store the current content in the history so it can be restored later
            self.history.add(get_current_content());
        }

        if *idx == 0 {
            return None;
        }

        *idx = (*idx).saturating_sub(1);

        self.history.get(*idx)
    }

    pub fn history_down(&self) -> Option<String> {
        let mut idx = self.history_idx.borrow_mut();
        *idx = (*idx + 1).clamp(0, self.history.len());

        if *idx == self.history.len() - 1 {
            // remove the current content from the history
            self.history.pop()
        } else {
            self.history.get(*idx)
        }
    }
}

impl Keybinding for EditorReader<'_> {
    fn read(&self, editor: &mut Editor<impl Renderer>) -> compose_editor::Result<bool> {
        let event = crossterm::event::read()?;

        let e = match event {
            Event::Key(key) => key,
            _ => return Ok(true),
        };

        let shifted = e.modifiers.contains(KeyModifiers::SHIFT);
        let control = e.modifiers.contains(KeyModifiers::CONTROL);

        match e.code {
            KeyCode::Enter if shifted => editor.type_char('\n'),
            KeyCode::Enter => return Ok(false),
            KeyCode::Down => {
                if editor.focus.ln + 1 < editor.line_count() {
                    editor.move_down(shifted)
                } else {
                    if let Some(s) = self.history_down() {
                        editor.set_contents(s.as_bytes())?;
                        editor.focus.ln = 0;
                        editor.move_to_line_end(false);
                    }
                }
            }
            KeyCode::Up => {
                if editor.focus.ln > 0 {
                    editor.move_up(shifted)
                } else {
                    if let Some(s) = self.history_up(|| editor.contents()) {
                        editor.set_contents(s.as_bytes())?;
                        editor.focus.ln = 0;
                        editor.move_to_line_end(false);
                    }
                }
            }
            KeyCode::Left => editor.move_left(shifted),
            KeyCode::Right => editor.move_right(shifted),

            KeyCode::PageDown => editor.move_to_bottom(),
            KeyCode::PageUp => editor.move_to_top(),
            KeyCode::Home => {
                let leading_spaces = editor
                    .curr_ln_chars()
                    .take_while(|c| c.is_whitespace())
                    .count();
                if editor.focus.col == leading_spaces {
                    editor.move_to_col(0, shifted);
                } else {
                    editor.move_to_col(leading_spaces, shifted);
                }
            }
            KeyCode::End => editor.move_to_line_end(shifted),

            KeyCode::Backspace => editor.backspace(),
            KeyCode::Char('h') if control => editor.backspace(),
            KeyCode::Delete => editor.delete(),
            KeyCode::Tab => {
                editor.clamp();
                let soft = 4 - editor.focus.col % 4;
                for _ in 0..soft {
                    editor.insert_char(0, ' ');
                }
                editor.focus.col += soft;
            }
            KeyCode::Esc => return Ok(false),

            KeyCode::Char(c) => editor.type_char(c),

            _ => { /* ignored */ }
        }

        Ok(true)
    }
}
