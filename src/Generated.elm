module Generated exposing (..)


type alias WorkExperience =
    { title : String
    , location : String
    , description : String
    , date : String
    }


workExperiences : List WorkExperience
workExperiences =
    [ { title = "Haskell/Cardano Blockchain Consultant"
      , location = "remote"
      , description = "MLabs"
      , date = "Aug 2021 - present"
      }
    , { title = "Full Stack Web Engineer"
      , location = "remote"
      , description = "Kakekomu Inc."
      , date = "Jan 2018 - Jul 2021"
      }
    , { title = "Frontend Web Engineer"
      , location = "Tokyo, Japan"
      , description = "Yahoo Japan"
      , date = "Jan 2018 - Mar 2019"
      }
    , { title = "Junior Software Engineer"
      , location = "Tokyo, Japan"
      , description = "Happiness Technology"
      , date = "Apr 2017 - Dec 2017"
      }
    ]


type alias Project =
    { title : String
    , stack : String
    , description : String
    , url : String
    }


projects : List Project
projects =
    [ { title = "LambdaBuffers"
      , stack = "Haskell / Rust / Nix"
      , description = "Schema language and code generator for polyglot projects"
      , url = "https://github.com/mlabs-haskell/lambda-buffers"
      }
    , { title = "Plutus Ledger API"
      , stack = "Rust"
      , description = "Plutus types and useful tools for Cardano dApp development"
      , url = "https://github.com/mlabs-haskell/plutus-ledger-api-rust"
      }
    , { title = "Tx Village"
      , stack = "Rust"
      , description = "Rust based toolkit for Cardano transaction building, verification and chain-indexing"
      , url = "https://github.com/mlabs-haskell/tx-village"
      }
    , { title = "Bot Plutus Interface"
      , stack = "Haskell"
      , description = "Transaction builder framework based on plutus-apps Contract monad interface"
      , url = "https://github.com/mlabs-haskell/bot-plutus-interface"
      }
    , { title = "Plutip"
      , stack = "Haskell"
      , description = "Cardano local test network executer and test framework"
      , url = "https://github.com/mlabs-haskell/plutip"
      }
    , { title = "Cardano Devnet Flake"
      , stack = "Nix"
      , description = "Cardano local test network executer based on process-compose"
      , url = "https://github.com/mlabs-haskell/cardano-devnet-flake"
      }
    , { title = "Flake-lang"
      , stack = "Nix"
      , description = "Nix tools powering polyglot mono-repositories"
      , url = "https://github.com/mlabs-haskell/flake-lang.nix"
      }
    , { title = "Szabo Gergely portfolio"
      , stack = "Elm"
      , description = "Personal portfolio web page, simulating a terminal with a few commands like `echo` and `figlet`."
      , url = "https://github.com/szg251/szabo-gergely-portfolio"
      }
    , { title = "Activity Analyser"
      , stack = "Rust"
      , description = "FIT file analyser for cycling activities"
      , url = "https://github.com/szg251/activity-analyser"
      }
    , { title = "Chess Clock"
      , stack = "Rust (embedded)"
      , description = "Chess clock implemented for an STM32 microcontroller"
      , url = "https://github.com/szg251/chesschock"
      }
    ]


type alias Skill =
    { title : String
    , description : String
    }


skills : List Skill
skills =
    [ { title = "Programming languages"
      , description = "I have several years of experience using Rust, Haskell, PureScript and TypeScript, but also worked on projects with Elm, Ruby and Python."
      }
    , { title = "Backend development"
      , description = "Designed and built REST and gRPC backend services with the following libraries (e.g. Rocket, Axum, Tonic, Servant, IHP, Koa.js, Ruby on Rails.)"
      }
    , { title = "Databases"
      , description = "Mostly but not exclusively used relational databases like PostgreSQL for backend applications and blockchain indexers."
      }
    , { title = "Blockchain dApp development"
      , description = "Designed and implemented several Cardano dApps, including sidechains, voting protoocols, etc."
      }
    , { title = "Frontend development"
      , description = "Worked as a full stack engineer maintaining large scale frontend and backend applications, using React (TypeScript) and Elm."
      }
    , { title = "DevOps"
      , description = "Configured and maintained projects using Nix (Hercules CI), bare metal Linux, AWS (ECS, Aurora, Lambda, S3, etc.), GCP"
      }
    , { title = "Agentic Development"
      , description = "Used LLMs (e.g. Claude, Codex) to supplement my engineering workflows and to help my research on new topics, or create quick prototypes or PoCs."
      }
    ]
