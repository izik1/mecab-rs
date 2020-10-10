#![allow(non_camel_case_types)]

mod dictionary;
// mod model;
pub mod node;
mod tagger;

use std::{
    ffi::{CStr, CString},
    os::raw::*,
    ptr, str,
};

type size_t = usize;

pub const MECAB_NOR_NODE: i32 = 0;
pub const MECAB_UNK_NODE: i32 = 1;
pub const MECAB_BOS_NODE: i32 = 2;
pub const MECAB_EOS_NODE: i32 = 3;
pub const MECAB_EON_NODE: i32 = 4;

pub const MECAB_SYS_DIC: i32 = 0;
pub const MECAB_USR_DIC: i32 = 1;
pub const MECAB_UNK_DIC: i32 = 2;

pub const MECAB_ONE_BEST: i32 = 1;
pub const MECAB_NBEST: i32 = 2;
pub const MECAB_PARTIAL: i32 = 4;
pub const MECAB_MARGINAL_PROB: i32 = 8;
pub const MECAB_ALTERNATIVE: i32 = 16;
pub const MECAB_ALL_MORPH: i32 = 32;
pub const MECAB_ALLOCATE_SENTENCE: i32 = 64;

pub const MECAB_ANY_BOUNDARY: i32 = 0;
pub const MECAB_TOKEN_BOUNDARY: i32 = 1;
pub const MECAB_INSIDE_TOKEN: i32 = 2;

#[repr(C)]
pub(crate) struct RawTagger {
    _private: [u8; 0],
}

#[allow(dead_code)]
#[repr(C)]
pub(crate) struct RawModel {
    _private: [u8; 0],
}

#[allow(dead_code)]
#[link(name = "mecab")]
extern "C" {
    fn mecab_new2(arg: *const c_char) -> *mut RawTagger;
    fn mecab_version() -> *const c_char;

    /// Return last error string.
    /// @return error string
    fn mecab_strerror(mecab: *mut RawTagger) -> *const c_char;

    /// delete Tagger object.
    /// This method calles "delete tagger".
    /// In some environment, e.g., MS-Windows, an object allocated inside a DLL must be deleted in the same DLL too.
    /// @param tagger tagger object
    fn mecab_destroy(mecab: *mut RawTagger);

    /// Return true if partial parsing mode is on.
    /// This method is DEPRECATED. Use Lattice::has_request_type(MECAB_PARTIAL).
    /// @return boolean
    fn mecab_get_partial(mecab: *const RawTagger) -> c_int;

    /// set partial parsing mode.
    /// This method is DEPRECATED. Use Lattice::add_request_type(MECAB_PARTIAL) or Lattice::remove_request_type(MECAB_PARTIAL)
    /// @param partial partial mode
    fn mecab_set_partial(mecab: *mut RawTagger, partial: c_int);

    /// Return temparature parameter theta.
    /// @return temparature parameter.
    fn mecab_get_theta(mecab: *const RawTagger) -> c_float;

    /// Set temparature parameter theta.
    /// @param theta temparature parameter.
    fn mecab_set_theta(mecab: *mut RawTagger, theta: c_float);

    /// Return lattice level.
    /// This method is DEPRECATED. Use Lattice::*_request_type()
    /// @return int lattice level
    fn mecab_get_lattice_level(mecab: *const RawTagger) -> c_int;

    /// Set lattice level.
    /// This method is DEPRECATED. Use Lattice::*_request_type()
    /// @param level lattice level
    fn mecab_set_lattice_level(mecab: *mut RawTagger, level: c_int);

    /// set all-morphs output mode.
    /// This method is DEPRECATED. Use Lattice::add_request_type(MECAB_ALL_MORPHS) or Lattice::remove_request_type(MECAB_ALL_MORPHS)
    /// @param all_morphs
    fn mecab_get_all_morphs(mecab: *const RawTagger) -> c_int;

    /// set all-morphs output mode.
    /// This method is DEPRECATED. Use Lattice::add_request_type(MECAB_ALL_MORPHS) or Lattice::remove_request_type(MECAB_ALL_MORPHS)
    /// @param all_morphs
    fn mecab_set_all_morphs(mecab: *mut RawTagger, all_morphs: c_int);

    /// Parse lattice object.
    /// Return true if lattice is parsed successfully.
    /// A sentence must be set to the lattice with Lattice:set_sentence object before calling this method.
    /// Parsed node object can be obtained with Lattice:bos_node.
    /// This method is thread safe.
    /// @return lattice lattice object
    /// @return boolean
    fn mecab_parse_lattice(mecab: *const RawTagger, lattice: *mut c_void) -> c_int;

    /// Parse given sentence and return parsed result as string.
    /// You should not delete the returned string. The returned buffer
    /// is overwritten when parse method is called again.
    /// This method is NOT thread safe.
    #[allow(dead_code)] // keep the function here for documentation purposes.
    fn mecab_sparse_tostr(mecab: *mut RawTagger, str: *const c_char) -> *const c_char;

    /// The same as `mecab_sparse_tostr`, but with input length (bytes) passed.
    fn mecab_sparse_tostr2(mecab: *mut RawTagger, str: *const c_char, len: size_t)
        -> *const c_char;

    /// Parse given sentence and return Node object.
    /// You should not delete the returned node object. The returned buffer
    /// is overwritten when parse method is called again.
    /// You can traverse all nodes via `Node::next` member.
    /// This method is NOT thread safe.
    /// @param str sentence
    /// @return bos node object
    #[allow(dead_code)] // keep the function here for documentation purposes.
    fn mecab_sparse_tonode<'a>(mecab: *mut RawTagger, str: *const c_char) -> *const Node<'a>;

    /// The same as parseToNode(), but input length can be passed
    fn mecab_sparse_tonode2<'a>(
        mecab: *mut RawTagger,
        str: *const c_char,
        len: size_t,
    ) -> *const Node<'a>;

    /// Parse given sentence and obtain N-best results as a string format.
    /// Currently, N must be 1 <= N <= 512 due to the limitation of the buffer size.
    /// You should not delete the returned string. The returned buffer
    /// is overwritten when parse method is called again.
    /// This method is DEPRECATED. Use Lattice class.
    /// @param N how many results you want to obtain
    /// @param str sentence
    /// @return parsed result
    #[allow(dead_code)] // keep the function here for documentation purposes.
    fn mecab_nbest_sparse_tostr(
        mecab: *mut RawTagger,
        N: size_t,
        str: *const c_char,
    ) -> *const c_char;

    /// The same as parseNBest(), but input length can be passed.
    /// @param N how many results you want to obtain
    /// @param str sentence
    /// @param len sentence length
    /// @return parsed result
    fn mecab_nbest_sparse_tostr2(
        mecab: *mut RawTagger,
        N: size_t,
        str: *const c_char,
        len: size_t,
    ) -> *const c_char;

    /// Initialize N-best enumeration with a sentence.
    /// Return true if initialization finishes successfully.
    /// N-best result is obtained by calling next() or nextNode() in sequence.
    /// This method is NOT thread safe.
    /// This method is DEPRECATED. Use Lattice class.
    /// @param str sentence
    /// @return boolean
    #[allow(dead_code)] // keep the function here for documentation purposes.
    fn mecab_nbest_init(mecab: *mut RawTagger, str: *const c_char) -> c_int;

    /// The same as parseNBestInit(), but input length can be passed.
    /// @param str sentence
    /// @param len sentence length
    /// @return boolean
    /// @return parsed result
    fn mecab_nbest_init2(mecab: *mut RawTagger, str: *const c_char, len: size_t) -> c_int;

    /// Return next-best parsed result. You must call parseNBestInit() in advance.
    /// Return NULL if no more reuslts are available.
    /// This method is NOT thread safe.
    /// This method is DEPRECATED. Use Lattice class.
    /// @return parsed result
    fn mecab_nbest_next_tostr(mecab: *mut RawTagger) -> *const c_char;

    /// Return next-best parsed result. You must call parseNBestInit() in advance.
    /// Return NULL if no more reuslt is available.
    /// This method is NOT thread safe.
    /// This method is DEPRECATED. Use Lattice class.
    /// @return node object
    fn mecab_nbest_next_tonode<'a>(mecab: *mut RawTagger) -> *const Node<'a>;

    fn mecab_format_node(mecab: *mut RawTagger, node: *const Node) -> *const c_char;

    /// Return DictionaryInfo linked list.
    /// @return DictionaryInfo linked list
    /// Safety:
    /// Rust has no way of knowing weather the inferred lifetime is correct.
    fn mecab_dictionary_info<'a>(mecab: *const RawTagger) -> *const DictionaryInfo<'a>;

    fn mecab_lattice_new() -> *mut c_void;
    fn mecab_lattice_destroy(lattice: *mut c_void);
    fn mecab_lattice_clear(lattice: *mut c_void);
    fn mecab_lattice_is_available(lattice: *mut c_void) -> c_int;
    fn mecab_lattice_get_bos_node<'a>(lattice: *mut c_void) -> *const Node<'a>;
    fn mecab_lattice_get_eos_node<'a>(lattice: *mut c_void) -> *const Node<'a>;
    fn mecab_lattice_get_begin_nodes<'a>(lattice: *mut c_void, pos: size_t) -> *const Node<'a>;
    fn mecab_lattice_get_end_nodes<'a>(lattice: *mut c_void, pos: size_t) -> *const Node<'a>;
    fn mecab_lattice_get_sentence(lattice: *mut c_void) -> *const c_char;
    fn mecab_lattice_set_sentence(lattice: *mut c_void, sentence: *const c_char);
    fn mecab_lattice_get_size(lattice: *mut c_void) -> size_t;
    fn mecab_lattice_get_z(lattice: *mut c_void) -> c_double;
    fn mecab_lattice_set_z(lattice: *mut c_void, Z: c_double);
    fn mecab_lattice_get_theta(lattice: *mut c_void) -> c_double;

    /// Set temparature parameter theta.
    /// @param theta temparature parameter.
    fn mecab_lattice_set_theta(lattice: *mut c_void, theta: c_double);

    fn mecab_lattice_next(lattice: *mut c_void) -> c_int;
    fn mecab_lattice_get_request_type(lattice: *mut c_void) -> c_int;
    fn mecab_lattice_has_request_type(lattice: *mut c_void, request_type: c_int) -> c_int;
    fn mecab_lattice_set_request_type(lattice: *mut c_void, request_type: c_int);
    fn mecab_lattice_add_request_type(lattice: *mut c_void, request_type: c_int);
    fn mecab_lattice_remove_request_type(lattice: *mut c_void, request_type: c_int);
    fn mecab_lattice_tostr(lattice: *mut c_void) -> *const c_char;
    fn mecab_lattice_nbest_tostr(lattice: *mut c_void, N: i64) -> *const c_char;
    fn mecab_lattice_has_constraint(lattice: *mut c_void) -> c_int;
    fn mecab_lattice_get_boundary_constraint(lattice: *mut c_void, pos: u64) -> c_int;
    fn mecab_lattice_get_feature_constraint(lattice: *mut c_void, pos: u64) -> *const c_char;
    fn mecab_lattice_set_boundary_constraint(lattice: *mut c_void, pos: u64, boundary_type: i32);
    fn mecab_lattice_set_feature_constraint(
        lattice: *mut c_void,
        begin_pos: u64,
        end_pos: u64,
        feature: *const c_char,
    );
    fn mecab_lattice_set_result(lattice: *mut c_void, result: *const c_char);
    fn mecab_lattice_strerror(lattice: *mut c_void) -> *const c_char;

    /// Factory method to create a new Model with a string parameter representation, i.e.,
    /// "-d /user/local/mecab/dic/ipadic -Ochasen".
    /// Return NULL if new model cannot be initialized. Use MeCab::getLastError() to obtain the
    /// cause of the errors.
    /// @return new Model object
    /// @param arg single string representation of the argment.
    fn mecab_model_new2(arg: *const c_char) -> *mut RawModel;

    /// delete Model object.
    /// This method calles "delete model".
    /// In some environment, e.g., MS-Windows, an object allocated inside a DLL must be deleted in the same DLL too.
    /// @param model model object
    fn mecab_model_destroy(model: *mut RawModel);

    /// Create a new Tagger object.
    /// All returned tagger object shares this model object as a parsing model.
    /// Never delete this model object before deleting tagger object.
    /// @return new Tagger object
    fn mecab_model_new_tagger(model: *const RawModel) -> *mut RawTagger;
    fn mecab_model_new_lattice(model: *mut RawModel) -> *mut c_void;

    /// Swap the instance with |model|.
    /// The ownership of |model| always moves to this instance,
    /// meaning that passed |model| will no longer be accessible after calling this method.
    /// return true if new model is swapped successfully.
    /// This method is thread safe. All taggers created by
    /// Model::createTagger() method will also be updated asynchronously.
    /// No need to stop the parsing thread excplicitly before swapping model object.
    /// @return boolean
    /// @param model new model which is going to be swapped with the current model.
    fn mecab_model_swap(model: *mut RawModel, new_model: *mut RawModel) -> c_int;

    /// Return DictionaryInfo linked list.
    /// @return DictionaryInfo linked list
    /// Safety:
    /// Rust has no way of knowing weather the inferred lifetime is correct.
    fn mecab_model_dictionary_info<'a>(model: *const RawModel) -> *const DictionaryInfo<'a>;

    /// Return transtion cost from rcAttr to lcAttr.
    /// @return transtion cost
    fn mecab_model_transition_cost(
        model: *const RawModel,
        rcAttr: c_ushort,
        lcAttr: c_ushort,
    ) -> c_int;

    fn mecab_model_lookup<'a>(
        model: *mut RawModel,
        begin: *const c_char,
        end: *const c_char,
        lattice: *mut c_void,
    ) -> *const Node<'a>;
}

use dictionary::DictionaryInfo;
// pub use model::Model;
use node::Node;
pub use tagger::Tagger;

// todo: it seems that mecab just uses a `static` string here, so we can probably return a `&'static str`
pub fn version() -> String {
    unsafe { ptr_to_string(mecab_version()) }
}

// todo: see if the unique reference requirement can be made more lax.
/// # Safety
/// `mecab` must be a valid reference *or* a null pointer.
/// `mecab` must be uniquely referenced for at least the chosen lifetime `'a`
/// Note: For a null pointer it isn't clear how long the reference should be valid, this is a possible source of unsoundness.
unsafe fn last_error<'a>(mecab: *mut RawTagger) -> Option<&'a CStr> {
    let err = mecab_strerror(mecab);
    if !err.is_null() {
        Some(CStr::from_ptr(err))
    } else {
        None
    }
}

pub fn global_last_error() -> Option<CString> {
    // safety: `ptr::null_mut` is a valid input to this function.
    // there *might* be a soundness issue with having the returned reference at all (including when and before it was created.)
    unsafe {
        last_error(ptr::null_mut())
            .filter(|s| !s.to_bytes().is_empty())
            .map(ToOwned::to_owned)
    }
}

fn str_to_ptr(input: &CString) -> *const i8 {
    input.as_ptr()
}

fn ptr_to_string(ptr: *const c_char) -> String {
    unsafe {
        let cstr = CStr::from_ptr(ptr);
        match str::from_utf8(cstr.to_bytes()) {
            Ok(s) => s.to_owned(),
            Err(e) => panic!("decoding {:?} failed: {}", cstr, e),
        }
    }
}
