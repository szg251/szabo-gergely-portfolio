#import "@preview/modern-cv:0.10.0": *

#let my-resume-entry(..args) = {
}

= Work Experience

#for entry in yaml("experience.yaml") {
  metadata(entry) 
  let items = entry.remove("items")

  resume-entry(..entry)
  resume-item[
    #for item in items [
      - #item
    ]
  ]
}


