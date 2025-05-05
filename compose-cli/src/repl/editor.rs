use minime::crossterm;
use minime::crossterm::event::{Event, KeyCode, KeyModifiers};
use minime::crossterm::style::{Colorize, Styler};
use minime::crossterm::terminal::{Clear, ClearType};
use minime::crossterm::QueueableCommand;
use minime::editor::keybindings::Keybinding;
use minime::editor::Editor;
use minime::renderer::styles::{Footer, Margin};
use minime::renderer::{RenderData, Renderer};
use std::io::Write;

pub fn print_input(input: &str, line_offset: usize) {
    for (i, line) in input.lines().enumerate() {
        println!(
            "{start_indicator}{line_no:>width$} {line}",
            line_no = i + 1 + line_offset,
            width = MARGIN_WIDTH - 3,
            start_indicator = if i == 0 {
                ">>".to_string()
            } else {
                "  ".to_string()
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

    fn draw(&mut self, write: &mut W, line_idx: usize, data: &RenderData) -> minime::Result<()> {
        let style = if line_idx == data.focus().ln {
            |str: String| str.white().bold()
        } else {
            |str: String| str.white()
        };
        write!(
            write,
            "{start_indicator}{:>width$} ",
            style((line_idx + 1 + self.line_offset).to_string()),
            width = MARGIN_WIDTH - 3,
            start_indicator = style(if line_idx == 0 {
                ">>".to_string()
            } else {
                "  ".to_string()
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

    fn draw(&mut self, write: &mut W, data: &RenderData) -> minime::Result<()> {
        write!(
            write,
            " ╰──── [{}:{}] {message}",
            data.focus().ln,
            data.focus().col.min(data.current_line().len()),
            message = self.message,
        )?;
        write.queue(Clear(ClearType::UntilNewLine))?;
        Ok(())
    }
}

pub struct KeyBindings;

impl Keybinding for KeyBindings {
    fn read(&self, editor: &mut Editor<impl Renderer>) -> minime::Result<bool> {
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
            KeyCode::Down => editor.move_down(shifted),
            KeyCode::Up => editor.move_up(shifted),
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
