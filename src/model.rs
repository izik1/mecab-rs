use std::{
    ffi::{c_void, CString},
    ptr::{self, NonNull},
};

use crate::{DictionaryInfo, Lattice, Node, Tagger};

pub struct Model {
    inner: NonNull<c_void>,
}

impl Model {
    /// Factory method to create a new Model with a string parameter representation, i.e.,
    /// "-d /user/local/mecab/dic/ipadic -Ochasen".
    /// Return NULL if new model cannot be initialized. Use MeCab::getLastError() to obtain the
    /// cause of the errors.
    pub fn new(args: &str) -> Result<Model, Option<CString>> {
        unsafe {
            let inner = NonNull::new(crate::mecab_model_new2(crate::str_to_ptr(
                &CString::new(args).unwrap(),
            )));

            let inner = match inner {
                Some(inner) => inner,
                None => return Err(crate::global_last_error()),
            };

            Ok(Model { inner })
        }
    }

    // todo: probably unsound
    pub fn create_tagger(&self) -> Option<Tagger> {
        unsafe {
            let inner = NonNull::new(crate::mecab_model_new_tagger(self.inner.as_ptr()))?;

            Some(Tagger::from_ptr(inner))
        }
    }

    // todo: probably unsound
    pub fn create_lattice(&self) -> Lattice {
        unsafe {
            Lattice {
                inner: crate::mecab_model_new_lattice(self.inner.as_ptr()),
                input: ptr::null(),
            }
        }
    }

    /// Swap the instance with `model`.
    /// The ownership of `model` always moves to this instance,
    /// meaning that passed |model| will no longer be accessible after calling this method.
    /// return true if new model is swapped successfully.
    /// This method is thread safe. All taggers created by
    /// `Model::create_tagger` will also be updated asynchronously.
    /// No need to stop the parsing thread excplicitly before swapping model object.
    // todo: probably unsound
    pub fn swap(&self, model: Model) -> bool {
        unsafe { crate::mecab_model_swap(self.inner.as_ptr(), model.inner.as_ptr()) != 0 }
    }

    // todo: probably unsound
    pub fn dictionary_info(&self) -> DictionaryInfo {
        unsafe { DictionaryInfo::new(crate::mecab_model_dictionary_info(self.inner.as_ptr())) }
    }

    // todo: probably unsound
    pub fn transition_cost(&self, rc_attr: u16, lc_attr: u16) -> i32 {
        unsafe { crate::mecab_model_transition_cost(self.inner.as_ptr(), rc_attr, lc_attr) }
    }

    // todo: probably unsound
    pub fn lookup<'a>(&'a self, begin: &str, len: u64, lattice: &'a Lattice) -> Option<Node<'a>> {
        unsafe {
            let raw_node = crate::mecab_model_lookup(
                self.inner.as_ptr(),
                crate::str_to_heap_ptr(begin),
                crate::str_to_heap_ptr(begin).offset(len as isize),
                lattice.inner,
            );
            Node::from_raw(raw_node)
        }
    }
}

impl Drop for Model {
    fn drop(&mut self) {
        unsafe {
            crate::mecab_model_destroy(self.inner.as_ptr());
        }
    }
}
