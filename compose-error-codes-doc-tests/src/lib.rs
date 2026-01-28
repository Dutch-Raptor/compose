/*!
Documentation and doc-tests for the Compose error codes.

The purpose of this crate is to provide the compose error codes as a Rust module, so that they can be included in docs.rs
and the examples can be tested.

The source of the error codes is in the [`compose-error-codes`] crate. The description of the error code is transformed
via `compose_doc::transform_markdown` into documented items with doc-tests.
*/

include!(concat!(env!("OUT_DIR"), "/Error_Codes"));
