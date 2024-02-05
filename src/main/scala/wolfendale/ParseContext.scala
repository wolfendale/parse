package wolfendale

abstract class ParseContext:

  type CollectionType[A]
  type NonEmptyCollectionType[A]

class DefaultParseContext extends ParseContext:

  override type CollectionType[A] = Seq[A]
  override type NonEmptyCollectionType[A] = Seq[A]


object ParseContext:

  given ParseContext = DefaultParseContext()