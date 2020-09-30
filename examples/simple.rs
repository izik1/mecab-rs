extern crate mecab;

use mecab::{Lattice, Node, Tagger};

fn main() {
    dbg!(std::mem::size_of::<Node>());

    let input = "太郎は次郎が持っている本を花子に渡した。";
    println!("INPUT: {}", input);

    let mut tagger =
        Tagger::new("-d /home/cr/Downloads/unidic-csj-3.0.1.1").expect("failed to create tagger");

    // gets tagged result as str
    let result = tagger.parse_to_str(input).expect("failed to parse input");
    println!("RESULT: {}", result);

    // gets N best results as String
    let result = tagger.parse_nbest(3, input);
    println!("NBEST:\n{}", result);

    // gets N best in sequence
    tagger.parse_nbest_init(input);
    for i in 0..3 {
        if let Some(res) = tagger.next() {
            println!("{}:\n{}", i, res);
        }
    }

    for node in tagger.parse_to_node(input).iter_next() {
        match node.stat as i32 {
            mecab::MECAB_BOS_NODE => {
                print!("{} BOS ", node.id);
            }
            mecab::MECAB_EOS_NODE => {
                print!("{} EOS ", node.id);
            }
            _ => {
                print!(
                    "{} {} ",
                    node.id,
                    &(node.surface())[..(node.length as usize)]
                );
            }
        }

        println!(
            "{} {} {} {} {} {} {} {} {} {} {} {} {}",
            node.feature(),
            input.len() as isize - node.surface().len() as isize,
            input.len() as isize - node.surface().len() as isize + node.length as isize,
            node.rcattr,
            node.lcattr,
            node.posid,
            node.char_type,
            node.stat,
            node.isbest,
            node.alpha,
            node.beta,
            node.prob,
            node.cost
        );
    }

    // dictionary info
    for dict in tagger.dictionary_info().iter() {
        println!();
        println!("filename: {}", dict.filename);
        println!("charset: {}", dict.charset);
        println!("size: {}", dict.size);
        println!("type: {}", dict.dict_type);
        println!("lsize: {}", dict.lsize);
        println!("rsize: {}", dict.rsize);
        println!("version: {}", dict.version);
    }
}
