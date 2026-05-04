use std::{fmt::Display, fs};

use indoc::formatdoc;

fn main() -> anyhow::Result<()> {
    let work_experiences_file = fs::read("../resume/sections/experience.yaml")?;
    let work_experiences: Vec<WorkExperience> = serde_yml::from_slice(&work_experiences_file)?;

    let projects_file = fs::read("../resume/sections/projects.yaml")?;
    let projects: Vec<Project> = serde_yml::from_slice(&projects_file)?;

    let skills_file = fs::read("../resume/sections/skills.yaml")?;
    let skills: Vec<Skill> = serde_yml::from_slice(&skills_file)?;

    let elm_module = [
        module_header(),
        work_experience_elm_type(),
        work_experiences_section(work_experiences),
        project_elm_type(),
        projects_section(projects),
        skill_elm_type(),
        skills_section(skills),
    ]
    .join("\n\n");

    fs::write("../src/Generated.elm", elm_module)?;
    Ok(())
}

fn module_header() -> String {
    formatdoc!(
        r#"
        module Generated exposing (..)
    "#
    )
}

#[derive(Debug, Clone, serde::Deserialize)]
struct WorkExperience {
    title: String,
    location: String,
    description: String,
    date: String,
    // items: Vec<String>,
}

fn work_experience_elm_type() -> String {
    formatdoc!(
        r#"
        type alias WorkExperience =
            {{ title : String
            , location : String
            , description : String
            , date : String
            }}
        "#
    )
}

fn work_experiences_section(work_experiences: Vec<WorkExperience>) -> String {
    let mut func = Doc::new(formatdoc!(
        r#"
        workExperiences : List WorkExperience
        workExperiences ="#
    ));

    let mut body = Doc::from_list(
        work_experiences
            .iter()
            .map(
                |WorkExperience {
                     title,
                     location,
                     description,
                     date,
                 }| {
                    record_body(vec![
                        format!(r#"title = "{title}""#),
                        format!(r#"location = "{location}""#),
                        format!(r#"description = "{description}""#),
                        format!(r#"date = "{date}""#),
                    ])
                },
            )
            .collect::<Vec<_>>(),
    )
    .add_ident();

    func.append(&mut body);

    func.to_string()
}

#[derive(Debug, Clone, serde::Deserialize)]
struct Project {
    title: String,
    stack: String,
    description: String,
    url: String,
}

fn project_elm_type() -> String {
    formatdoc!(
        r#"
        type alias Project =
            {{ title : String
            , stack : String
            , description : String
            , url : String
            }}
        "#
    )
}

fn projects_section(projects: Vec<Project>) -> String {
    let mut func = Doc::new(formatdoc!(
        r#"
        projects : List Project
        projects ="#
    ));

    let mut body = Doc::from_list(
        projects
            .iter()
            .map(
                |Project {
                     title,
                     stack,
                     description,
                     url,
                 }| {
                    record_body(vec![
                        format!(r#"title = "{title}""#),
                        format!(r#"stack = "{stack}""#),
                        format!(r#"description = "{description}""#),
                        format!(r#"url = "{url}""#),
                    ])
                },
            )
            .collect::<Vec<_>>(),
    )
    .add_ident();

    func.append(&mut body);

    func.to_string()
}

#[derive(Debug, Clone, serde::Deserialize)]
struct Skill {
    title: String,
    description: String,
}

fn skill_elm_type() -> String {
    formatdoc!(
        r#"
        type alias Skill =
            {{ title : String
            , description : String
            }}
        "#
    )
}

fn skills_section(skills: Vec<Skill>) -> String {
    let mut func = Doc::new(formatdoc!(
        r#"
        skills : List Skill
        skills ="#
    ));

    let mut body = Doc::from_list(
        skills
            .iter()
            .map(|Skill { title, description }| {
                record_body(vec![
                    format!(r#"title = "{title}""#),
                    format!(r#"description = "{description}""#),
                ])
            })
            .collect::<Vec<_>>(),
    )
    .add_ident();

    func.append(&mut body);

    func.to_string()
}

fn record_body(mut items: Vec<String>) -> Doc {
    items.iter_mut().enumerate().for_each(|(i, str)| {
        if i == 0 {
            str.insert_str(0, "{ ");
        } else {
            str.insert_str(0, ", ");
        }
    });
    items.push(String::from("}"));
    Doc(items)
}

#[derive(Default)]
struct Doc(Vec<String>);

impl Doc {
    fn new(str: String) -> Self {
        Self(vec![str])
    }

    fn add_ident(self) -> Self {
        Doc(self
            .0
            .into_iter()
            .map(|mut str| {
                str.insert_str(0, "    ");
                str
            })
            .collect())
    }

    fn from_list(items: Vec<Doc>) -> Doc {
        let mut lines = items
            .into_iter()
            .enumerate()
            .flat_map(|(i, Doc(inner))| {
                inner
                    .into_iter()
                    .enumerate()
                    .map(|(j, mut str)| {
                        if j == 0 {
                            let delimit = if i == 0 { "[ " } else { ", " };
                            str.insert_str(0, delimit);
                        } else {
                            str.insert_str(0, "  ");
                        }
                        str
                    })
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();

        lines.push(String::from("]"));
        Doc(lines)
    }

    fn append(&mut self, other: &mut Self) {
        self.0.append(&mut other.0);
    }
}

impl Display for Doc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut str = self.0.join("\n");
        str.push('\n');
        write!(f, "{}", str)
    }
}
