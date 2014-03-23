package gkit.mongo

import gkit.Generator

import reactivemongo.bson.BSONObjectID

trait GeneratorInstances {

  implicit def BSONObjectIDGenerator = new Generator[BSONObjectID] {
    def generate = BSONObjectID.generate
  }
}
