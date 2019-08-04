# Mr. Boilerplate

This is a code generator with a little webapp UI to help generate tedious, Scala boilerplate.

Try it online right now here: https://japgolly.github.io/mr.boilerplate/


# Why?

Sometimes boilerplate is simply unavoidable.

Macros can be a pretty good solution but there are trade-offs that can be(come) unacceptable.

Codecs are a great example because they define external interfaces,
and the fact that your own code compiles and passes its tests doesn't mean that you haven't
broken the protocol with external people or services.
Consider a simple example of someone writing a JSON codec.
The evolution in a real-world project usually goes something like this...

```scala
import io.circe.generic.auto._

// or

import io.circe.generic.semiauto._
implicit val decoderPerson: Decoder[Person] = deriveDecoder[Person]
implicit val encoderPerson: Encoder[Person] = deriveEncoder[Person]
```

And then a few versions later, someone does a bit of refactoring, does a new deployment,
and suddenly people in your environment are saying things are broken.
Oh right, the refactor affected the field names, the macro happily revised the codec,
devs didn't realise.

*(Regardless of macro or not, it's a good habit to store real JSON blobs in your tests for
protocol stability. It's generally not feasible to do that for all possible paths but 80%+
is usually very easy and high value. But I digress...)*

Ok, we don't want things breaking when someone refactors things; let's make the protocols explicit.

```scala
implicit val decoderPerson: Decoder[Person] =
  Decoder.forProduct3("name", "address", "phone")(Person.apply)

implicit val encoderPerson: Encoder[Person] =
  Encoder.forProduct3("name", "address", "phone")(a => (a.name, a.address, a.phone))
```

That's better.
A few versions later we need to change the `name` field to `{"surname":xxx, "given":yyy}`
instead of just a string. We need to still be backwards-compatible though so now we
need to rewrite the whole thing like this:

```scala
implicit val decoderPerson: Decoder[Person] =
  Decoder.instance { c =>
    for {
      name    <- /* custom logic here*/
      address <- c.get[String]("address")
      phone   <- c.get[Option[String]]("phone")
    } yield Person(name, address, phone)
  }

implicit val encoderPerson: Encoder[Person] =
  Encoder.instance(value => Json.obj(
    "name"    -> /* custom logic here*/,
    "address" -> value.address.asJson,
    "phone"   -> value.phone.asJson,
  ))
```

It's a pain. Especially when there's a lot of it to do, or especially when the types are large.
With [Mr. Boilrerplate](https://japgolly.github.io/mr.boilerplate/) you can just paste in the data
definition, and immediately copy the `forProduct` codecs back out. In future when you need to start
adding custom logic to a codec, repaste the definition, click a checkbox for manual-style codecs,
and copy the expanded code back into your codebase. Big time saver.


# Nice Features

For monomorphic types, it generates simple `val`s:

```scala
implicit val decoder: Decoder[Person] =
  Decoder.forProduct3("name", "address", "phone")(Person.apply)
```

For polymorphic types, it generates `def`s with correct type parameters,
and type constraints and/or direct implicit evidences depending on the fields.

```scala
// case class NonEmptyList[+A](head: A, tail: List[A])
implicit def decoder[A: Decoder]: Decoder[NonEmptyList[A]] = // [A: Decoder] here
  Decoder.forProduct2("head", "tail")(NonEmptyList.apply[A]) // <-- .apply[A] here

// case class Roles[F[_], A](roles: F[A])
implicit def decoder[F[_], A](implicit ev1: Decoder[F[A]]): Decoder[Roles[F, A]] = // ev1 here
  Decoder[F[A]].map(Roles.apply[F, A]) // <-- .apply[F,A] here
```


# Can I add more generators, or generation options?

Absolutely! Submit a PR. Once merged I'll update the live site.

Generators are pretty simple to write. All the Scala type logic is done by the library.

Have a look at the [Circe generator source code](https://github.com/japgolly/mr.boilerplate/blob/master/core/shared/src/main/scala/japgolly/mrboilerplate/core/gen/Circe.scala)
and [its unit test](https://github.com/japgolly/mr.boilerplate/blob/master/core/shared/src/test/scala/japgolly/mrboilerplate/core/gen/CirceTest.scala).

It looks a bit big only because it can generate code in a number of different styles according to its options.