package record

import com.mongodb.{BasicDBList, BasicDBObject, DBObject, BasicDBObjectBuilder}
import DBObjectSerializer._
import record.signatures._

/**
 * Created by tim on 21.05.16.
 */
object MongoBuilder {

  def insertQueryAsString[C](i: InsertQuery[C]) = {
    "db.%s.insert(%s)".format(i.t, i.c.toString)
  }

  def insertQuery[F](i: InsertQuery[_]):execute[Boolean] = {
    insertExecute(i.t.toString, i.c.asInstanceOf[DBObject])
  }

  def whereQuery(s: WhereQuery[_]):execute[Boolean] = {
    conditionExecute(s.collection.toString, buildCondition(s.w.c))
  }

  def selectQuery[R](s: SelectQuery[R]):execute[R] = {

    def as(a:DBObject, e: Field[_,_]) = fromDBObject(a.get(e.fieldName), e.runtimeClass)

    s.s match {
      case e: se => se(s.c.toString, buildCondition(s.s.w), asObject[R](_, s.c.runtimeClass))
      case e: sf1 => sf(s.c.toString, buildCondition(s.s.w), buildSelectFields(s.s), a => fromDBObject(a.get(e.c1.fieldName), e.c1.runtimeClass).asInstanceOf[R])
      case e: sf2 => sf(s.c.toString, buildCondition(s.s.w), buildSelectFields(s.s), a => (as(a, e.c1), as(a, e.c2)).asInstanceOf[R])
      case e: sf3 => sf(s.c.toString, buildCondition(s.s.w), buildSelectFields(s.s), a => (as(a, e.c1), as(a, e.c2), as(a, e.c3)).asInstanceOf[R])
    }
  }

  def modifyQuery[F](s: ModifyQuery[_]):execute[Boolean] = {
    updateExecute(s.c.toString, buildCondition(s.s.condition.c), buildModify(s.s))
  }

  def selectQueryAsString(s: SelectQuery[_]) = {
    val fields = buildSelectFields(s.s)
    val condition = buildCondition(s.s.w).toString
    val collection = s.c
    if (fields.keySet().isEmpty) {
      "db.%s.find(%s)".format(collection, condition)
    } else {
      "db.%s.find(%s, %s)".format(collection, condition, fields)
    }
  }

  def modifyQueryAsString(u: ModifyQuery[_]) = {
    val collection = u.c
    val condition = buildCondition(u.s.condition.c).toString
    val modify = buildModify(u.s)
    "db.%s.update(%s, %s)".format(collection, condition, modify)
  }

  def buildJoin(joins: Seq[join]):Seq[DBObject] = {
    joins.map { join =>
      val builder = BasicDBObjectBuilder.start()
      builder.append("$lookup", BasicDBObjectBuilder.start()
        .append("from", join.joined.collection.toString)
        .append("localField", join.owner.toString)
        .append("foreignField", join.joined.toString)
        .append("as", join.joined.collection.toString).get()).get()
    }
  }

  def joinQueryAsString(join: JoinQuery[_]):String = {
    val dblist = new BasicDBList()
    buildJoin(join.joined.joinExpression).foreach {dblist.add}
    "db.%s.aggregate(%s)".format(join.joined, dblist.toString)
  }

  def joinQuery[R](join: JoinQuery[R]):execute[R] = {

    def relation(j: join, dbo: DBObject) = {
      val collection = dbo.get(j.joined.collection.toString).asInstanceOf[BasicDBList].toArray.toList
      j match {
        case y: joinOne =>
          collection.headOption.map(fromDBObject(_, y.joined.collection.asInstanceOf[M].runtimeClass))
        case y: joinMany =>
          collection.map(fromDBObject(_, y.joined.collection.asInstanceOf[M].runtimeClass))
      }
    }

    val condition =  new BasicDBObject("$match", buildCondition(join.joined.condition))
    val joins = buildJoin(join.joined.joinExpression)//TODO

    joinExecute[R](join.collection.toString, condition::joins.toList, { m =>

      val head = fromDBObject(m, join.collection.runtimeClass)

      join.joined match {
        case JoinStateYield1(c,j1) => (head, relation(j1, m)).asInstanceOf[R]
        case JoinStateYield2(c,j1, j2) => (head, relation(j1, m), relation(j2, m)).asInstanceOf[R]
      }

    })
  }

  def buildModify(modify: update):DBObject = {
    val builder = BasicDBObjectBuilder.start()
    modify.modify.foreach {
      s => s._1 match {
        case `setExpression` => builder.append("$set", buildListToDBObject(s._2))
        case `minExpression` => builder.append("$min", buildListToDBObject(s._2))
        case `maxExpression` => builder.append("$max", buildListToDBObject(s._2))
        case `renameExpression` => builder.append("$rename", buildListToDBObject(s._2))
        case `incExpression` => builder.append("$inc", buildListToDBObject(s._2))
        case `mulExpression` => builder.append("$mul", buildListToDBObject(s._2))
        case `unsetExpression` => builder.append("$unset", buildListToDBObject(s._2))
      }
    }
    builder.get()
  }

  def buildListToDBObject(list: List[update]) = {
    val builder = BasicDBObjectBuilder.start()
    list.foreach(a => builder.append(a.left.toString, a.right))
    builder.get()
  }

  def buildSelectFields[R](select: SelectState[R]) = {
    val builder = BasicDBObjectBuilder.start()
    select match {
      case e: sf1 => builder.append(e.c1.toString, 1)
      case _ =>
    }
    builder.get()
  }

  def buildCondition(predicate: Expression[_], builder: BasicDBObjectBuilder = BasicDBObjectBuilder.start()):DBObject = {
    predicate match {
      case b: BooleanExpression[_,_] =>
        b.operator match {
          case "$eq" => builder.append(b.left.toString, DBObjectSerializer.asDBObject(b.right, b.runtimeClass)).get
          case _ => builder.append(
            b.left.toString, new BasicDBObject(b.operator, DBObjectSerializer.asDBObject(b.right, b.runtimeClass))).get
        }
      case l: LogicalExpression[_] =>
        builder.append(l.operator, (buildCondition(l.left)::buildCondition(l.right)::Nil).toArray).get
      case allExpression() => new BasicDBObject()
    }
  }

}