use std::{
    ffi::c_void,
    ffi::CString,
    ptr::{self, NonNull},
};

use crate::{str_to_heap_ptr, str_to_ptr, DictionaryInfo, Lattice, Node, Tagger};

pub struct Model {
    inner: *mut c_void,
}

impl Model {
    pub fn new(args: &str) -> Model {
        unsafe {
            Model {
                inner: crate::mecab_model_new2(str_to_ptr(&CString::new(args).unwrap())),
            }
        }
    }

    pub fn create_tagger(&self) -> Option<Tagger> {
        unsafe {
            let inner = NonNull::new(crate::mecab_model_new_tagger(self.inner))?;

            Some(Tagger::from_ptr(inner))
        }
    }

    pub fn create_lattice(&self) -> Lattice {
        unsafe {
            Lattice {
                inner: crate::mecab_model_new_lattice(self.inner),
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
    pub fn swap(&self, model: Model) -> bool {
        unsafe { crate::mecab_model_swap(self.inner, model.inner) != 0 }
    }

    pub fn dictionary_info(&self) -> DictionaryInfo {
        unsafe { DictionaryInfo::new(crate::mecab_model_dictionary_info(self.inner)) }
    }

    pub fn transition_cost(&self, rc_attr: u16, lc_attr: u16) -> i32 {
        unsafe { crate::mecab_model_transition_cost(self.inner, rc_attr, lc_attr) }
    }

    pub fn lookup<'a>(&'a self, begin: &str, len: u64, lattice: &'a Lattice) -> Option<Node<'a>> {
        unsafe {
            let raw_node = crate::mecab_model_lookup(
                self.inner,
                str_to_heap_ptr(begin),
                str_to_heap_ptr(begin).offset(len as isize),
                lattice.inner,
            );
            Node::from_raw(raw_node)
        }
    }
}

impl Drop for Model {
    fn drop(&mut self) {
        unsafe {
            crate::mecab_model_destroy(self.inner);
        }
    }
}
