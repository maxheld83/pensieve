workflow "Build, Check, Document and Deploy" {
  on = "push"
  resolves = [
    "Upload Cache",
    "Code Coverage",
    "Deploy Website"
  ]
}

action "GCP Authenticate" {
  uses = "actions/gcloud/auth@ba93088eb19c4a04638102a838312bb32de0b052"
  secrets = [
    "GCLOUD_AUTH"
  ]
}

action "Download Cache" {
  uses = "actions/gcloud/cli@d124d4b82701480dc29e68bb73a87cfb2ce0b469"
  runs = "gsutil -m cp -r gs://pensieve/lib.tar.gz /github/home/"
  needs = [
    "GCP Authenticate"
  ]
}

action "Decompress Cache" {
  uses = "actions/bin/sh@5968b3a61ecdca99746eddfdc3b3aab7dc39ea31"
  runs = "tar --extract --ungzip --file /github/home/lib.tar.gz --directory /github/home"
  needs = [
    "Download Cache"
  ]
}

action "Install Dependencies" {
  uses = "r-lib/ghactions/actions/install-deps@66e2d1931618a56914201b1ea50b01ac7cacb7c0"
  needs = [
    "Decompress Cache"
  ]
}

action "Compress Cache" {
  uses = "actions/bin/sh@5968b3a61ecdca99746eddfdc3b3aab7dc39ea31"
  runs = "tar --gzip --create --file lib.tar.gz --directory /github/home lib"
  needs = [
    "Install Dependencies"
  ]
}

action "Document Package" {
  uses = "r-lib/ghactions/actions/document@66e2d1931618a56914201b1ea50b01ac7cacb7c0"
  needs = [
    "Install Dependencies"
  ]
}

action "Build Package" {
  uses = "r-lib/ghactions/actions/build@66e2d1931618a56914201b1ea50b01ac7cacb7c0"
  needs = [
    "Document Package"
  ]
}

action "Check Package" {
  uses = "r-lib/ghactions/actions/check@66e2d1931618a56914201b1ea50b01ac7cacb7c0"
  needs = [
    "Build Package"
  ]
}

action "Install Package" {
  uses = "r-lib/ghactions/actions/install@66e2d1931618a56914201b1ea50b01ac7cacb7c0"
  needs = [
    "Build Package"
  ]
}

action "Build Website" {
  uses = "r-lib/ghactions/actions/pkgdown@66e2d1931618a56914201b1ea50b01ac7cacb7c0"
  needs = [
    "Install Package"
  ]
}

action "Filter Not Act" {
  uses = "actions/bin/filter@3c0b4f0e63ea54ea5df2914b4fabf383368cd0da"
  args = "not actor nektos/act"
  needs = [
    "Check Package", 
    "Build Website"
  ]
}

action "Filter Master" {
  uses = "actions/bin/filter@c6471707d308175c57dfe91963406ef205837dbd"
  needs = [
    "Upload Cache",
    "Code Coverage"
  ]
  args = "branch master"
}

action "Upload Cache" {
  uses = "actions/gcloud/cli@d124d4b82701480dc29e68bb73a87cfb2ce0b469"
  runs = "gsutil -m cp lib.tar.gz gs://pensieve/"
  needs = [
    "Compress Cache",
    "Filter Not Act"
  ]
}

action "Code Coverage" {
  uses = "r-lib/ghactions/actions/covr@0c71330b5d9bca082cf47c7d21603659095f5034"
  needs = [
    "Filter Not Act"
  ]
  secrets = [
    "CODECOV_TOKEN"
  ]
}

action "Deploy Website" {
  uses = "maxheld83/ghpages@v0.1.1"
  env = {
    BUILD_DIR = "docs"
  }
  secrets = ["GH_PAT"]
  needs = [
    "Filter Master"
  ]
}
