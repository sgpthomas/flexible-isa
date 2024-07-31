use super::{ast, HalideType};

use pest_consume::{match_nodes, Parser};
use serde::{Deserialize, Serialize};

use std::{
    collections::HashMap,
    fs::{self, File},
    io::BufReader,
    path::{Path, PathBuf},
};

#[derive(Debug, Serialize, Deserialize)]
pub enum GeneratorIO {
    Input(GeneratorType, u64),
    Output(GeneratorType, u64),
}

impl GeneratorIO {
    fn dim(self, dim: u64) -> Self {
        match self {
            GeneratorIO::Input(typ, _) => GeneratorIO::Input(typ, dim),
            GeneratorIO::Output(typ, _) => GeneratorIO::Output(typ, dim),
        }
    }

    fn input(typ: GeneratorType) -> Self {
        GeneratorIO::Input(typ, 1)
    }

    fn output(typ: GeneratorType) -> Self {
        GeneratorIO::Output(typ, 1)
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub enum GeneratorType {
    Buffer(HalideType),
    Raw(HalideType),
}

impl From<GeneratorIO> for HalideType {
    fn from(value: GeneratorIO) -> HalideType {
        match value {
            GeneratorIO::Input(gen_typ, _) => gen_typ.into(),
            GeneratorIO::Output(gen_typ, _) => gen_typ.into(),
        }
    }
}

impl From<GeneratorType> for HalideType {
    fn from(value: GeneratorType) -> HalideType {
        match value {
            GeneratorType::Buffer(hal_typ) => HalideType::Ptr(vec![hal_typ.to_id()]),
            GeneratorType::Raw(hal_typ) => hal_typ,
        }
    }
}

// include the grammar file so that Cargo knows to rebuild this file on grammar changes
const _GRAMMR: &str = include_str!("generator_types.pest");

type ParseResult<T> = std::result::Result<T, pest_consume::Error<Rule>>;
type Node<'i> = pest_consume::Node<'i, Rule, ()>;

/// Parse the Input / Output declarations in a Halide generator file
#[derive(pest_consume::Parser)]
#[grammar = "halide_ir/generator_types.pest"]
pub struct HalideGeneratorParser;

impl HalideGeneratorParser {
    pub fn parse_file(path: &Path) -> anyhow::Result<Vec<(ast::Id, GeneratorIO)>> {
        let raw_content = fs::read(path)?;
        let string_content = std::str::from_utf8(&raw_content)?.to_string();

        let mut map = vec![];

        // parse file line by line
        // if the parser returns an error, this is not a Input/Output declaration
        // and so we can ignore it
        for line in string_content.lines() {
            if let Ok(inputs) = HalideGeneratorParser::parse(Rule::line, line) {
                let (id, gen_io) = HalideGeneratorParser::line(inputs.single().unwrap())?;
                map.push((id, gen_io));
            }
        }

        Ok(map)
    }

    pub fn halide_type_from(raw: &str) -> HalideType {
        match raw {
            "uint8_t" => HalideType::Unsigned(8),
            "uint16_t" => HalideType::Unsigned(16),
            "uint32_t" => HalideType::Unsigned(32),
            "uint64_t" => HalideType::Unsigned(64),
            "int8_t" => HalideType::Signed(8),
            "int16_t" => HalideType::Signed(16),
            "int32_t" => HalideType::Signed(32),
            "int64_t" => HalideType::Signed(64),
            _ => HalideType::Unknown,
        }
    }

    pub fn write_json(path: &Path) -> anyhow::Result<()> {
        let io = Self::parse_file(path)?;
        let mut output_path = PathBuf::from(
            path.file_stem()
                .unwrap()
                .to_str()
                .unwrap()
                .strip_suffix("_generator")
                .unwrap(),
        );
        output_path.set_extension("json");
        let mut file = File::create(output_path)?;
        serde_json::to_writer_pretty(&mut file, &io)?;
        Ok(())
    }

    pub fn read_json(path: &Path) -> anyhow::Result<HashMap<ast::Id, HalideType>> {
        let mut metadata_path = path.to_path_buf();
        metadata_path.set_extension("json");
        let metadata_file = File::open(metadata_path)?;
        let reader = BufReader::new(metadata_file);
        let func_sig: Vec<(ast::Id, GeneratorIO)> = serde_json::from_reader(reader)?;
        Ok(func_sig
            .into_iter()
            .map(|(id, gen_typ)| (id, gen_typ.into()))
            .collect())
    }
}

#[pest_consume::parser]
impl HalideGeneratorParser {
    fn identifier(input: Node) -> ParseResult<ast::Id> {
        Ok(ast::Id::new(input.as_str()))
    }

    fn char(input: Node) -> ParseResult<&str> {
        Ok(input.as_str())
    }

    fn string_lit(input: Node) -> ParseResult<String> {
        Ok(match_nodes!(
            input.into_children();
            [char(c)..] => c.collect::<Vec<_>>().join(""),
        ))
    }

    fn num(input: Node) -> ParseResult<u64> {
        input
            .as_str()
            .parse::<u64>()
            .map_err(|_| input.error("Expected valid u64"))
    }

    fn line(input: Node) -> ParseResult<(ast::Id, GeneratorIO)> {
        Ok(match_nodes!(
            input.into_children();
            [gen_io(gen_io), var_info(var), EOI(_)] => (var.0, gen_io.dim(var.1))
        ))
    }

    fn var_info(input: Node) -> ParseResult<(ast::Id, u64)> {
        Ok(match_nodes!(
            input.into_children();
            [identifier(_), string_lit(s), num(dim)] => (ast::Id::new(s), dim),
            [identifier(_), string_lit(s)] => (ast::Id::new(s), 1),
            [identifier(_), string_lit(_), num(_)..] => todo!("not sure if this is allowed yet"),
        ))
    }

    fn gen_io(input: Node) -> ParseResult<GeneratorIO> {
        Ok(match_nodes!(
            input.into_children();
            [input(_), halide_type(htyp)] => GeneratorIO::input(htyp),
            [output(_), halide_type(htyp)] => GeneratorIO::output(htyp)
        ))
    }

    fn input(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn output(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn halide_type(input: Node) -> ParseResult<GeneratorType> {
        Ok(match_nodes!(
            input.into_children();
            [buffer(_), identifier(id)] => GeneratorType::Buffer(Self::halide_type_from(&id.name)),
            [identifier(id)] => GeneratorType::Raw(Self::halide_type_from(&id.name)),
        ))
    }

    fn buffer(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn EOI(_input: Node) -> ParseResult<()> {
        Ok(())
    }
}
