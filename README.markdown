# README #

Text.XML.Generic can automatically serialize Data to Strings (a la XML).

The API is similar to Text.JSON.Generic.

## Serialize ##

There are two functions for serialization

    encodeXML :: Data a => a -> String

and

    toXML :: Data a => a -> Element

.

### Example ###

    encodeXML $ User { name = "Pelle", age = 34 }

turns into

    "<User><name><String>Pelle</String><age><Integer>34</Integer></age></User>"

while

    encodeXML $ Test "foo" "bar"

turns into

    "<Test><String>foo</String><String>bar</String></Test"

## Deserialize ##

There are two functions for deserialization.

    decodeXML :: Data a => String -> Either String a

and

    fromXML :: Data d => Element -> Either String d

.

### Example ###

    decodeXML "<User><name><String>Pelle</String><age><Integer>34</Integer></age></User>" :: User

turns into

    User { name = "Pelle", age = 34 }

while

    decodeXML "<Test><String>foo</String><String>bar</String></Test" :: Test

turns into

    Test "foo" "bar"
