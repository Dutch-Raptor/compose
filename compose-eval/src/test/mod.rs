use crate::{EvalConfig, Machine};
use compose_error_codes::ErrorCode;
use compose_library::diag::codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use compose_library::diag::{
    write_diagnostics, FileError, FileResult, SourceDiagnostic, SourceResult, Warned,
};
use compose_library::{library, Library, Value, World};
use compose_syntax::{FileId, Source};
use ecow::{eco_format, eco_vec, EcoVec};
use std::collections::HashMap;
use std::fmt::Debug;
use std::io::{Read, Write};
use std::sync::Mutex;
use tap::pipe::Pipe;

#[cfg(test)]
mod snippets;
#[cfg(test)]
mod iterators;

pub struct TestWorld {
    sources: Mutex<HashMap<FileId, Source>>,
    entrypoint: FileId,
    library: Library,
}

impl Clone for TestWorld {
    fn clone(&self) -> Self {
        Self {
            sources: Mutex::new(self.sources.lock().unwrap().clone()),
            entrypoint: self.entrypoint,
            library: self.library.clone(),
        }
    }
}

impl Debug for TestWorld {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // try to get the mutex lock

        f.debug_struct("TestWorld")
            .field("entrypoint", &self.entrypoint)
            .pipe(|d| {
                if let Ok(sources) = self.sources.try_lock() {
                    d.field("sources", &sources)
                } else {
                    d.field("sources", &"<locked>")
                }
            })
            .finish()
    }
}

impl TestWorld {
    pub fn from_str(text: &str) -> Self {
        let entrypoint = FileId::new("main.comp");
        let source = Source::new(entrypoint, text.to_string());
        let mut sources = HashMap::new();
        sources.insert(source.id(), source);

        Self {
            sources: Mutex::new(sources),
            entrypoint,
            library: library(),
        }
    }

    pub fn new() -> Self {
        Self::from_str("")
    }

    pub fn entrypoint_src(&self) -> Source {
        self.sources
            .lock()
            .unwrap()
            .get(&self.entrypoint)
            .unwrap()
            .clone()
    }

    pub fn edit_source(&self, file_id: FileId, editor: impl FnOnce(&mut Source)) {
        let mut sources = self.sources.lock().unwrap();
        let source = sources.get_mut(&file_id).unwrap();
        editor(source);
    }
}

impl World for TestWorld {
    fn entry_point(&self) -> FileId {
        self.entrypoint
    }

    fn source(&self, file_id: FileId) -> FileResult<Source> {
        let sources = self
            .sources
            .lock()
            .map_err(|e| FileError::Other(Some(eco_format!("{e}"))))?;
        match sources.get(&file_id) {
            Some(s) => Ok(s.clone()),
            None => Err(FileError::NotFound(file_id.path().0.clone())),
        }
    }

    fn library(&self) -> &Library {
        &self.library
    }

    fn write(&self, f: &dyn Fn(&mut dyn Write) -> std::io::Result<()>) -> std::io::Result<()> {
        f(&mut std::io::stdout())
    }

    fn read(&self, f: &dyn Fn(&mut dyn Read) -> std::io::Result<()>) -> std::io::Result<()> {
        f(&mut std::io::stdin())
    }
}

fn print_diagnostics(
    world: &TestWorld,
    errors: &[SourceDiagnostic],
    warnings: &[SourceDiagnostic],
) {
    let stdout = StandardStream::stdout(ColorChoice::Always);
    write_diagnostics(
        world,
        errors,
        warnings,
        &mut stdout.lock(),
        &Default::default(),
    )
    .expect("failed to print diagnostics");
}

#[must_use]
pub fn eval_code_with_vm(vm: &mut Machine, world: &TestWorld, input: &str) -> TestResult {
    if input.is_empty() {
        return TestResult {
            value: Ok(Value::unit()),
            warnings: eco_vec!(),
            world: world.clone(),
        };
    }

    let len_before_edit = world.entrypoint_src().nodes().len();
    world.edit_source(world.entry_point(), |s| {
        s.append(format!("{}{input}", if !s.text().is_empty() { "\n" } else { "" }).as_str())
    });

    let source = world.entrypoint_src();
    let len_after_edit = source.nodes().len();

    let Warned { value, warnings } = crate::eval_range(
        &source,
        len_before_edit..len_after_edit,
        vm,
        &EvalConfig {
            include_syntax_warnings: true,
        },
    );

    TestResult {
        value,
        warnings,
        world: world.clone(),
    }
}

pub struct TestResult {
    pub value: SourceResult<Value>,
    pub warnings: EcoVec<SourceDiagnostic>,
    pub world: TestWorld,
}

impl TestResult {
    #[track_caller]   
    pub fn assert_no_errors(self) -> Self {
        match &self.value {
            Ok(_) => {}
            Err(errors) => {
                print_diagnostics(&self.world, errors, &self.warnings);
                panic!("expected no errors, but got: {:?}", errors)
            }
        }
        self
    }

    #[track_caller]
    pub fn assert_no_warnings(self) -> Self {
        if !self.warnings.is_empty() {
            print_diagnostics(&self.world, &self.warnings, &self.warnings);
            panic!("expected no warnings, but got: {:?}", self.warnings);
        }
        self
    }

    #[track_caller]
    pub fn assert_errors(self, expected_errors: &[ErrorCode]) -> Self {
        match &self.value {
            Ok(_) => panic!("expected errors, but got none"),
            Err(errors) => {
                if expected_errors.is_empty() {
                    panic!("expected no errors, but got: {:?}", errors)   
                }
                if errors
                    .iter()
                    .map(|e| e.code)
                    .zip(expected_errors.iter().map(Some))
                    .any(|(a, b)| a != b)
                {
                    print_diagnostics(&self.world, errors, &self.warnings);
                    panic!(
                        "expected errors: {:?}, but got: {:?}",
                        expected_errors, errors
                    )
                }
            }
        }

        self
    }

    #[track_caller]
    pub fn assert_warnings(self, expected_warnings: &[ErrorCode]) -> Self {
        if self
            .warnings
            .iter()
            .map(|e| e.code)
            .zip(expected_warnings.iter().map(Some))
            .any(|(a, b)| a != b)
        {
            print_diagnostics(&self.world, &self.warnings, &self.warnings);
            panic!(
                "expected warnings: {:?}, but got: {:?}",
                expected_warnings, self.warnings
            )
        }

        self
    }

    pub fn get_value(self) -> Value {
        self.value.expect("code failed to evaluate")
    }

    #[allow(unused)]
    pub fn get_warnings(&self) -> EcoVec<SourceDiagnostic> {
        self.warnings.clone()
    }

    #[allow(unused)]
    pub fn get_errors(&self) -> EcoVec<SourceDiagnostic> {
        match &self.value {
            Ok(_) => eco_vec!(),
            Err(errors) => errors.clone(),
        }
    }
}

#[must_use]
pub fn eval_code(code: &str) -> TestResult {
    let world = TestWorld::from_str("");
    let mut vm = Machine::new(&world);
    eval_code_with_vm(&mut vm, &world, code)
}

#[track_caller]
pub fn assert_eval(code: &str) -> Value {
    eval_code(code)
        .assert_no_warnings()
        .assert_no_errors()
        .get_value()
}

#[track_caller]
pub fn assert_eval_with_vm(vm: &mut Machine, world: &TestWorld, code: &str) -> Value {
    eval_code_with_vm(vm, world, code)
        .assert_no_warnings()
        .assert_no_errors()
        .get_value()
}
