package org.example

object Modelling {

  case class Artist(
                   name: String,
                   genre: String,
                   origin: String,
                   yearsActiveStart: Int,
                   isActive: Boolean,
                   yearsActiveEnd: Int
                   )

  def searchArtists(
                   artists: List[Artist],
                   genres: List[String],
                   locations: List[String],
                   searchByActiveYears: Boolean,
                   activeAfter: Int,
                   activeBefore: Int
                   ): List[Artist] ={

  }


  object model {
    opaque type Location = String
    object Location {
      def apply(value: String): Location = value // <- you can use a String as a Location only in the scope of model
      extension(a: Location) def name: String = a
    }

    // Practicing newtypes
    opaque type Genre = String
    object Genre {
      def apply(value: String): Genre = value
      extension(a: Genre) def name: String = a
    }

    opaque type YearsActiveStart = Int
    object YearsActiveStart {
      def apply(value: Int): YearsActiveStart = value
      extension(a: YearsActiveStart) def value: Int = a
    }

    opaque type YearsActiveEnd = Int
    object YearsActiveEnd {
      def apply(value: Int): YearsActiveEnd = value
      extension(a: YearsActiveEnd) def value: Int = a
    }
  }
}
