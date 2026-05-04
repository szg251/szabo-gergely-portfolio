#import "@preview/modern-cv:0.10.0": *

#show: resume.with(
  author: (
    firstname: "Gergely",
    lastname: "Szabo",
    email: "szg251@mailbox.org",
    phone: "(+36) 30 979 0998",
    github: "szg251",
    linkedin: "szg251",
    address: "Gyor, Hungary",
    positions: (
      "Web Backend Developer",
      "Blockchain Developer",
    ),
    website: "https://www.szabogergely.com"
  ),
  profile-picture: none,
  date: datetime.today().display(),
  paper-size: "a4",
)

= Professional Summary

#resume-item[
  I am a Software Engineer mainly specialised in blockchain dApp and web backend development.

]

= Work Experience

#for entry in yaml("sections/experience.yaml") {
  let items = entry.remove("items")

  resume-entry(..entry)
  resume-item[
    #for item in items [
      - #item
    ]
  ]
}

= Projects

#for entry in yaml("sections/projects.yaml") {
  let url = entry.remove("url")
  let stack = entry.remove("stack")

  entry.insert("location", stack)

  resume-entry(..entry)
  resume-item[
    #link(url)
  ]
}

= Education

#resume-entry(
  title: "Kyoto University of Education",
  location: "Kyoto, Japan",
  date: "2013-2014",
  description: "Japanese Language and Cultural Studies Exchange Student",
)

#resume-entry(
  title: "A Tan Kapuja Buddhista College",
  location: "Budapest, Hungary",
  date: "2011 - 2015",
  description: "BA in Japanese Language and Culture",
)

= Skills

#for entry in yaml("sections/skills.yaml") {
  resume-skill-item(entry.title, (entry.description,))
}

= Languages

#resume-skill-item("Fluent", ("Hungarian (native)", "English (TOEIC 985)", "Japanese (JLPT N1)"))
#resume-skill-item("Beginner", ("French", "Italian"))

= Interests

#resume-item[
  road cycling, chess, programming, bass guitar (jazz/fusion), language learning
]
