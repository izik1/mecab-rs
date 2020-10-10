use std::{ffi::CString, ptr};

use ptr::NonNull;

use crate::{node::Node2, Node, RawLattice};

pub struct Lattice {
    // todo: figure out how to avoid this
    pub(in crate::tagger) inner: NonNull<RawLattice>,
}

impl Lattice {
    /// Create a new Lattice object
    pub fn new() -> Lattice {
        unsafe {
            Lattice {
                inner: NonNull::new(crate::mecab_lattice_new())
                    .expect("mecab bug: `mecab_lattice_new` returned null"),
            }
        }
    }

    /// Clear all internal lattice data.
    pub fn clear(&mut self) {
        unsafe {
            crate::mecab_lattice_clear(self.inner.as_ptr());
        }
    }

    /// Return true if result object is available.
    pub fn is_available(&self) -> bool {
        unsafe { crate::mecab_lattice_is_available(self.inner.as_ptr()) != 0 }
    }

    /// Return bos (begin of sentence) node.
    pub fn bos_node<'a>(&'a self) -> Option<&'a Node2<'a>> {
        unsafe {
            // safety: needs `&'a self`
            let res = (crate::mecab_lattice_get_bos_node(self.inner.as_ptr()));

            match res.is_null() {
                true => None,
                // safety: this deref is fine because we know the pointer isn't null, and we can only assume it's valid otherwise.
                false => Some(&*res),
            }
            .expect("BOS node should always be valid")
        }
    }

    // todo: potentially unsound.
    pub fn eos_node<'a>(&'a self) -> Node<'a> {
        unsafe {
            Node::from_raw(crate::mecab_lattice_get_eos_node(self.inner))
                .expect("EOS node should always be valid")
        }
    }

    // todo: potentially unsound.
    pub fn begin_nodes<'a>(&'a self, pos: usize) -> Option<Node<'a>> {
        unsafe {
            let raw_node = crate::mecab_lattice_get_begin_nodes(self.inner, pos);
            Node::from_raw(raw_node)
        }
    }

    // todo: potentially unsound.
    pub fn end_nodes<'a>(&'a self, pos: usize) -> Option<Node<'a>> {
        unsafe {
            let raw_node = crate::mecab_lattice_get_end_nodes(self.inner, pos);
            Node::from_raw(raw_node)
        }
    }

    // todo: potentially unsound.
    pub fn sentence(&self) -> String {
        unsafe { crate::ptr_to_string(crate::mecab_lattice_get_sentence(self.inner)) }
    }

    // todo: potentially unsound.
    pub fn set_sentence<T: Into<Vec<u8>>>(&mut self, sentence: T) {
        unsafe {
            self.free_input();
            self.input = crate::str_to_heap_ptr(sentence);
            crate::mecab_lattice_set_sentence(self.inner, self.input);
        }
    }

    pub fn size(&self) -> usize {
        // safety: this is just a simple property get on the C side.
        unsafe { crate::mecab_lattice_get_size(self.inner.as_ptr()) }
    }

    // todo: potentially unsound.
    pub fn z(&self) -> f64 {
        unsafe { crate::mecab_lattice_get_z(self.inner) }
    }

    // todo: potentially unsound.
    pub fn set_z(&self, z: f64) {
        unsafe {
            crate::mecab_lattice_set_z(self.inner, z);
        }
    }

    // todo: potentially unsound.
    pub fn theta(&self) -> f64 {
        unsafe { crate::mecab_lattice_get_theta(self.inner) }
    }

    // todo: potentially unsound.
    pub fn set_theta(&self, theta: f64) {
        unsafe {
            crate::mecab_lattice_set_theta(self.inner, theta);
        }
    }

    // todo: potentially unsound.
    pub fn next(&self) -> bool {
        unsafe { crate::mecab_lattice_next(self.inner) != 0 }
    }

    // todo: potentially unsound.
    pub fn request_type(&self) -> i32 {
        unsafe { crate::mecab_lattice_get_request_type(self.inner) }
    }

    // todo: potentially unsound.
    pub fn has_request_type(&self, request_type: i32) -> bool {
        unsafe { crate::mecab_lattice_has_request_type(self.inner, request_type) != 0 }
    }

    // todo: potentially unsound.
    pub fn set_request_type(&self, request_type: i32) {
        unsafe {
            crate::mecab_lattice_set_request_type(self.inner, request_type);
        }
    }

    // todo: potentially unsound.
    pub fn add_request_type(&self, request_type: i32) {
        unsafe {
            crate::mecab_lattice_add_request_type(self.inner, request_type);
        }
    }

    // todo: potentially unsound.
    pub fn remove_request_type(&self, request_type: i32) {
        unsafe {
            crate::mecab_lattice_remove_request_type(self.inner, request_type);
        }
    }

    // todo: potentially unsound.
    pub fn to_string(&self) -> String {
        unsafe { crate::ptr_to_string(crate::mecab_lattice_tostr(self.inner)) }
    }

    // todo: potentially unsound.
    pub fn enum_nbest_as_string(&self, n: i64) -> String {
        unsafe { crate::ptr_to_string(crate::mecab_lattice_nbest_tostr(self.inner, n)) }
    }

    // todo: potentially unsound.
    pub fn has_constraint(&self) -> bool {
        unsafe { crate::mecab_lattice_has_constraint(self.inner) != 0 }
    }

    // todo: potentially unsound.
    pub fn boundary_constraint(&self, pos: u64) -> i32 {
        unsafe { crate::mecab_lattice_get_boundary_constraint(self.inner, pos) }
    }

    // todo: potentially unsound.
    pub fn feature_constraint(&self, pos: u64) -> String {
        unsafe {
            crate::ptr_to_string(crate::mecab_lattice_get_feature_constraint(self.inner, pos))
        }
    }

    // todo: potentially unsound.
    pub fn set_boundary_constraint(&self, pos: u64, boundary_type: i32) {
        unsafe {
            crate::mecab_lattice_set_boundary_constraint(self.inner, pos, boundary_type);
        }
    }

    // todo: Go into mecab source to figure out how to decode this.
    // // todo: potentially unsound.
    // pub fn set_feature_constraint<T: Into<Vec<u8>>>(
    //     &self,
    //     begin_pos: u64,
    //     end_pos: u64,
    //     feature: T,
    // ) {
    //     unsafe {
    //         crate::mecab_lattice_set_feature_constraint(
    //             self.inner,
    //             begin_pos,
    //             end_pos,
    //             str_to_ptr(&CString::new(feature).unwrap()),
    //         );
    //     }
    // }

    // todo: potentially unsound.
    pub fn set_result<T: Into<Vec<u8>>>(&self, result: T) {
        unsafe { mecab_lattice_set_result(self.inner, str_to_ptr(&CString::new(result).unwrap())) }
    }

    // todo: potentially unsound.
    pub fn what(&self) -> String {
        unsafe { ptr_to_string(mecab_lattice_strerror(self.inner)) }
    }
}

impl Drop for Lattice {
    fn drop(&mut self) {
        unsafe {
            crate::mecab_lattice_destroy(self.inner.as_ptr());
        }
    }
}

impl Default for Lattice {
    fn default() -> Self {
        Self::new()
    }
}
