workflow "Build, Check, Document and Deploy" {
  on = "push"
  resolves = [
    "Build Image",
    "Document Package",
    "Code Coverage",
    "Deploy",
    "Build Package",
  ]
}

action "Build Image" {
  uses = "actions/docker/cli@c08a5fc9e0286844156fefff2c141072048141f6"
  args = "build --tag=repo:latest ."
}

action "Build Package" {
  needs = "Build Image"
  uses = "maxheld83/ghactions/Rscript-byod@master"
  args = "-e 'devtools::build()'"
}

action "Check Package" {
  uses = "maxheld83/ghactions/Rscript-byod@master"
  needs = ["Build Package"]
  args = "-e 'devtools::check_built(path = \".\", error_on = \"warning\")'"
}

action "Document Package" {
  uses = "maxheld83/ghactions/Rscript-byod@master"
  needs = ["Build Package"]
  args = "-e 'devtools::install(); pkgdown::build_site()'"
}

action "Code Coverage" {
  uses = "maxheld83/ghactions/Rscript-byod@master"
  needs = ["Build Package"]
  args = "-e 'covr::codecov()'"
  secrets = ["CODECOV_TOKEN"]
}

action "Master Branch" {
  uses = "actions/bin/filter@c6471707d308175c57dfe91963406ef205837dbd"
  needs = ["Check Package", "Document Package"]
  args = "branch master"
}

action "Deploy" {
  needs = [
    "Master Branch"
  ]
  uses = "w9jds/firebase-action@v1.0.1"
  args = [
    "deploy --only hosting"
  ]
  env = {
    PROJECT_ID = "pensieve-169bf"
  }
  secrets = [
    "FIREBASE_TOKEN"
  ]
}
