#![allow(non_camel_case_types)]

mod dictionary;
// mod model;
pub mod node;
mod tagger;

use std::{
    default::Default,
    ffi::{CStr, CString},
    os::raw::*,
    ptr::{self, NonNull},
    str,
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

#[repr(C)]
pub(crate) struct RawModel {
    _private: [u8; 0],
}

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
    fn mecab_sparse_tonode(mecab: *mut RawTagger, str: *const c_char) -> *const raw_node;

    /// The same as parseToNode(), but input length can be passed
    fn mecab_sparse_tonode2(
        mecab: *mut RawTagger,
        str: *const c_char,
        len: size_t,
    ) -> *const raw_node;

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
    fn mecab_nbest_next_tonode(mecab: *mut RawTagger) -> *const raw_node;

    fn mecab_format_node(mecab: *mut RawTagger, node: *const Node2) -> *const c_char;

    /// Return DictionaryInfo linked list.
    /// @return DictionaryInfo linked list
    /// Safety:
    /// Rust has no way of knowing weather the inferred lifetime is correct.
    fn mecab_dictionary_info<'a>(mecab: *const RawTagger) -> *const DictionaryInfo<'a>;

    fn mecab_lattice_new() -> *mut c_void;
    fn mecab_lattice_destroy(lattice: *mut c_void);
    fn mecab_lattice_clear(lattice: *mut c_void);
    fn mecab_lattice_is_available(lattice: *mut c_void) -> c_int;
    fn mecab_lattice_get_bos_node(lattice: *mut c_void) -> *mut raw_node;
    fn mecab_lattice_get_eos_node(lattice: *mut c_void) -> *mut raw_node;
    fn mecab_lattice_get_begin_nodes(lattice: *mut c_void, pos: size_t) -> *const raw_node;
    fn mecab_lattice_get_end_nodes(lattice: *mut c_void, pos: size_t) -> *const raw_node;
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

    fn mecab_model_lookup(
        model: *mut RawModel,
        begin: *const c_char,
        end: *const c_char,
        lattice: *mut c_void,
    ) -> *const raw_node;
}

use dictionary::DictionaryInfo;
// pub use model::Model;
use node::Node2;
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
pub struct Lattice {
    inner: *mut c_void,
    input: *const i8,
}

impl Lattice {
    pub fn new() -> Lattice {
        unsafe {
            Lattice {
                inner: mecab_lattice_new(),
                input: ptr::null(),
            }
        }
    }

    // UB: drops self.input without borrow
    // UB: drops self.input while "something" requiring it may still be alive.
    fn free_input(&self) {
        unsafe {
            if !self.input.is_null() {
                CString::from_raw(self.input as *mut i8);
            }
        }
    }

    pub fn clear(&self) {
        unsafe {
            mecab_lattice_clear(self.inner);
            self.free_input();
        }
    }

    pub fn is_available(&self) -> bool {
        unsafe { mecab_lattice_is_available(self.inner) != 0 }
    }

    pub fn bos_node<'a>(&'a self) -> Node<'a> {
        unsafe {
            Node::from_raw(mecab_lattice_get_bos_node(self.inner))
                .expect("BOS node should always be valid")
        }
    }

    pub fn eos_node<'a>(&'a self) -> Node<'a> {
        unsafe {
            Node::from_raw(mecab_lattice_get_eos_node(self.inner))
                .expect("EOS node should always be valid")
        }
    }

    pub fn begin_nodes<'a>(&'a self, pos: usize) -> Option<Node<'a>> {
        unsafe {
            let raw_node = mecab_lattice_get_begin_nodes(self.inner, pos);
            Node::from_raw(raw_node)
        }
    }

    pub fn end_nodes<'a>(&'a self, pos: usize) -> Option<Node<'a>> {
        unsafe {
            let raw_node = mecab_lattice_get_end_nodes(self.inner, pos);
            Node::from_raw(raw_node)
        }
    }

    pub fn sentence(&self) -> String {
        unsafe { ptr_to_string(mecab_lattice_get_sentence(self.inner)) }
    }

    pub fn set_sentence<T: Into<Vec<u8>>>(&mut self, sentence: T) {
        unsafe {
            self.free_input();
            self.input = str_to_heap_ptr(sentence);
            mecab_lattice_set_sentence(self.inner, self.input);
        }
    }

    pub fn size(&self) -> usize {
        unsafe { mecab_lattice_get_size(self.inner) }
    }

    pub fn z(&self) -> f64 {
        unsafe { mecab_lattice_get_z(self.inner) }
    }

    pub fn set_z(&self, z: f64) {
        unsafe {
            mecab_lattice_set_z(self.inner, z);
        }
    }

    pub fn theta(&self) -> f64 {
        unsafe { mecab_lattice_get_theta(self.inner) }
    }

    pub fn set_theta(&self, theta: f64) {
        unsafe {
            mecab_lattice_set_theta(self.inner, theta);
        }
    }

    pub fn next(&self) -> bool {
        unsafe { mecab_lattice_next(self.inner) != 0 }
    }

    pub fn request_type(&self) -> i32 {
        unsafe { mecab_lattice_get_request_type(self.inner) }
    }

    pub fn has_request_type(&self, request_type: i32) -> bool {
        unsafe { mecab_lattice_has_request_type(self.inner, request_type) != 0 }
    }

    pub fn set_request_type(&self, request_type: i32) {
        unsafe {
            mecab_lattice_set_request_type(self.inner, request_type);
        }
    }

    pub fn add_request_type(&self, request_type: i32) {
        unsafe {
            mecab_lattice_add_request_type(self.inner, request_type);
        }
    }

    pub fn remove_request_type(&self, request_type: i32) {
        unsafe {
            mecab_lattice_remove_request_type(self.inner, request_type);
        }
    }

    pub fn to_string(&self) -> String {
        unsafe { ptr_to_string(mecab_lattice_tostr(self.inner)) }
    }

    pub fn enum_nbest_as_string(&self, n: i64) -> String {
        unsafe { ptr_to_string(mecab_lattice_nbest_tostr(self.inner, n)) }
    }

    pub fn has_constraint(&self) -> bool {
        unsafe { mecab_lattice_has_constraint(self.inner) != 0 }
    }

    pub fn boundary_constraint(&self, pos: u64) -> i32 {
        unsafe { mecab_lattice_get_boundary_constraint(self.inner, pos) }
    }

    pub fn feature_constraint(&self, pos: u64) -> String {
        unsafe { ptr_to_string(mecab_lattice_get_feature_constraint(self.inner, pos)) }
    }

    pub fn set_boundary_constraint(&self, pos: u64, boundary_type: i32) {
        unsafe {
            mecab_lattice_set_boundary_constraint(self.inner, pos, boundary_type);
        }
    }

    pub fn set_feature_constraint<T: Into<Vec<u8>>>(
        &self,
        begin_pos: u64,
        end_pos: u64,
        feature: T,
    ) {
        unsafe {
            mecab_lattice_set_feature_constraint(
                self.inner,
                begin_pos,
                end_pos,
                str_to_ptr(&CString::new(feature).unwrap()),
            );
        }
    }

    pub fn set_result<T: Into<Vec<u8>>>(&self, result: T) {
        unsafe { mecab_lattice_set_result(self.inner, str_to_ptr(&CString::new(result).unwrap())) }
    }

    pub fn what(&self) -> String {
        unsafe { ptr_to_string(mecab_lattice_strerror(self.inner)) }
    }
}

impl Drop for Lattice {
    fn drop(&mut self) {
        unsafe {
            mecab_lattice_destroy(self.inner);
            self.free_input();
        }
    }
}

impl Default for Lattice {
    fn default() -> Self {
        Self::new()
    }
}

#[repr(C)]
struct raw_node {
    prev: *mut raw_node,
    next: *mut raw_node,
    enext: *mut raw_node,
    bnext: *mut raw_node,
    rpath: *mut c_void,
    lpath: *mut c_void,
    surface: *const c_char,
    feature: *const c_char,
    id: c_uint,
    length: c_ushort,
    rlength: c_ushort,
    rcattr: c_ushort,
    lcattr: c_ushort,
    posid: c_ushort,
    char_type: c_uchar,
    stat: c_uchar,
    isbest: c_uchar,
    alpha: c_float,
    beta: c_float,
    prob: c_float,
    wcost: c_short,
    cost: c_long,
}

enum Mode {
    NEXT,
    PREV,
    ENEXT,
    BNEXT,
}

pub struct NodeIter<'a> {
    current: Option<Node<'a>>,
    mode: Mode,
}

impl<'a> Iterator for NodeIter<'a> {
    type Item = Node<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let old = self.current.take()?;

        self.current = match self.mode {
            Mode::NEXT => old.next(),
            Mode::PREV => old.prev(),
            Mode::ENEXT => old.enext(),
            Mode::BNEXT => old.bnext(),
        };

        Some(old)
    }
}

#[derive(Clone)]
pub struct Node<'a> {
    inner: &'a raw_node,

    pub id: u32,
    pub length: u16,
    pub rlength: u16,
    pub rcattr: u16,
    pub lcattr: u16,
    pub posid: u16,
    pub char_type: u8,
    pub stat: u8,
    pub isbest: bool,
    pub alpha: f32,
    pub beta: f32,
    pub prob: f32,
    pub wcost: i16,
    // use long instead of Rust integer as its size may by different
    pub cost: c_long,
}

impl<'a> Node<'a> {
    #[inline]
    pub fn surface(&self) -> &'a str {
        // Safety: Safe because we validate the utf-8 when initializing.
        unsafe { str::from_utf8_unchecked(CStr::from_ptr(self.inner.surface).to_bytes()) }
    }

    /// The part of surface covered by *this* node.
    #[inline]
    pub fn visible(&self) -> &'a str {
        &self.surface()[..self.length as usize]
    }

    #[inline]
    pub fn feature(&self) -> &'a str {
        // Safety: Safe because we validate the utf-8 when initializing.
        unsafe { str::from_utf8_unchecked(CStr::from_ptr(self.inner.feature).to_bytes()) }
    }

    fn from_raw(raw_ptr: *const raw_node) -> Option<Self> {
        unsafe {
            let raw_ptr = NonNull::new(raw_ptr as *mut _)?;
            let raw_node: &raw_node = &*raw_ptr.as_ptr();

            if !validate_str(raw_node.surface) {
                return None;
            }

            if !validate_str(raw_node.feature) {
                return None;
            }

            Some(Node {
                inner: raw_node,
                id: raw_node.id,
                length: raw_node.length,
                rlength: raw_node.rlength,
                rcattr: raw_node.rcattr,
                lcattr: raw_node.lcattr,
                posid: raw_node.posid,
                char_type: raw_node.char_type,
                stat: raw_node.stat,
                isbest: raw_node.isbest != 0,
                alpha: raw_node.alpha,
                beta: raw_node.beta,
                prob: raw_node.prob,
                wcost: raw_node.wcost,
                cost: raw_node.cost,
            })
        }
    }

    pub fn iter_prev(self) -> NodeIter<'a> {
        NodeIter {
            current: Some(self),
            mode: Mode::PREV,
        }
    }

    pub fn prev(&self) -> Option<Node<'a>> {
        Node::from_raw(self.inner.prev)
    }

    pub fn iter_next(self) -> NodeIter<'a> {
        NodeIter {
            current: Some(self),
            mode: Mode::NEXT,
        }
    }

    pub fn next(&self) -> Option<Node<'a>> {
        Node::from_raw(self.inner.next)
    }

    pub fn iter_enext(self) -> NodeIter<'a> {
        NodeIter {
            current: Some(self),
            mode: Mode::ENEXT,
        }
    }

    pub fn enext(&self) -> Option<Node<'a>> {
        Node::from_raw(self.inner.enext)
    }

    pub fn iter_bnext(self) -> NodeIter<'a> {
        NodeIter {
            current: Some(self),
            mode: Mode::BNEXT,
        }
    }

    pub fn bnext(&self) -> Option<Node<'a>> {
        Node::from_raw(self.inner.bnext)
    }
}

fn str_to_ptr(input: &CString) -> *const i8 {
    input.as_ptr()
}

fn str_to_heap_ptr<T: Into<Vec<u8>>>(input: T) -> *mut i8 {
    CString::new(input).unwrap().into_raw()
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

fn validate_str(ptr: *const c_char) -> bool {
    unsafe { str::from_utf8(CStr::from_ptr(ptr).to_bytes()).is_ok() }
}
