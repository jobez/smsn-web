let mkPackage =
	  https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.2-20190725/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
	  https://github.com/purescript/package-sets/releases/download/psc-0.13.5-20200103/packages.dhall sha256:0a6051982fb4eedb72fbe5ca4282259719b7b9b525a4dda60367f98079132f30

let overrides =
	  { halogen =
		  upstream.halogen ⫽ { version = "v5.0.0-rc.5" }
	  , halogen-vdom =
		  upstream.halogen-vdom ⫽ { version = "v6.1.0" }
	  , websocket =
		  upstream.web-socket ⫽ { version = "v2.0.0" }
	  , effect =
		  upstream.effect ⫽ { version = "v2.0.1" }
	  }

let additions =
	  { logging =
		  mkPackage
		  [ "prelude"
		  , "contravariant"
		  , "console"
		  , "effect"
		  , "transformers"
		  , "tuples"
		  , "either"
		  ]
		  "https://github.com/rightfold/purescript-logging.git"
		  "v3.0.0"
	  , halogen-formless =
		  mkPackage
		  [ "halogen"
		  , "variant"
		  , "heterogeneous"
		  , "generics-rep"
		  , "profunctor-lenses"
		  ]
		  "https://github.com/thomashoneyman/purescript-halogen-formless.git"
		  "v1.0.0-rc.1"
	  , slug =
		  mkPackage
		  [ "prelude"
		  , "maybe"
		  , "strings"
		  , "unicode"
		  , "generics-rep"
		  , "argonaut-codecs"
		  ]
		  "https://github.com/thomashoneyman/purescript-slug.git"
		  "v1.0.0"
	  , precise-datetime =
		  mkPackage
		  [ "arrays"
		  , "console"
		  , "datetime"
		  , "either"
		  , "enums"
		  , "foldable-traversable"
		  , "formatters"
		  , "integers"
		  , "js-date"
		  , "lists"
		  , "maybe"
		  , "newtype"
		  , "prelude"
		  , "strings"
		  , "tuples"
		  , "unicode"
		  , "numbers"
		  , "decimals"
		  ]
		  "https://github.com/awakesecurity/purescript-precise-datetime.git"
		  "v5.1.1"
	  , higher-order =
		  mkPackage
		  [ "catenable-lists"
		  , "const"
		  , "effect"
		  , "errors"
		  , "generics-rep"
		  , "lists"
		  , "ordered-collections"
		  , "orders"
		  , "profunctor"
		  ]
		  "https://github.com/matthew-hilty/purescript-higher-order.git"
		  "v0.2.0"
	  , proxying =
		  mkPackage
		  [ "console"
		  , "effect"
		  , "generics-rep"
		  , "prelude"
		  , "test-unit"
		  , "typelevel-prelude"
		  ]
		  "https://github.com/matthew-hilty/purescript-proxying.git"
		  "v1.1.0"
	  , struct =
		  mkPackage
		  [ "argonaut"
		  , "argonaut-codecs"
		  , "console"
		  , "effect"
		  , "proxying"
		  , "record"
		  , "record-extra"
		  , "subcategory"
		  , "test-unit"
		  , "variant"
		  ]
		  "https://github.com/matthew-hilty/purescript-struct.git"
		  "v1.1.0"
	  , subcategory =
		  mkPackage
		  [ "prelude", "profunctor", "record" ]
		  "https://github.com/matthew-hilty/purescript-subcategory.git"
		  "v0.2.0"
	  , tolerant-argonaut =
		  mkPackage
		  [ "argonaut-codecs"
		  , "argonaut-core"
		  , "arrays"
		  , "console"
		  , "effect"
		  , "higher-order"
		  , "lists"
		  , "psci-support"
		  , "record"
		  , "struct"
		  , "typelevel-prelude"
		  ]
		  "https://github.com/matthew-hilty/purescript-tolerant-argonaut.git"
		  "v1.1.0"
	  }

in  upstream ⫽ overrides ⫽ additions
