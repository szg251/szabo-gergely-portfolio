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

#include "sections/experience.typ"
#include "sections/education.typ"
#include "sections/projects.typ"
#include "sections/other.typ"
