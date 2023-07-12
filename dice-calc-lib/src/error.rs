use thiserror::Error;

#[derive(Error, Debug)]
pub enum CalcError {
    #[error("failed to parse input: {0}")]
    ParseFailure(String),
    #[error("{arg} does not have expected format: {details}")]
    UnexpectedArgument { arg: String, details: String },
    #[error("`help` can only be placed as the first token and should be called with `help()` fn")]
    HelpMustBeCalled,
}
