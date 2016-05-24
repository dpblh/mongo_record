package record

import com.mongodb.{BasicDBList, BasicDBObject, DBObject, BasicDBObjectBuilder}
import record.signatures._

/**
 * Created by tim on 21.05.16.
 */
object MongoBuilder {

  def insertQuery[F](i: InsertQuery[_]):execute[Boolean] = {
    insertExecute(i.t.toString, i.c.asInstanceOf[DBObject])
  }

  def insertQueryAsString[C](i: InsertQuery[C]) = {
    "db.%s.insert(%s)".format(i.t, i.c.toString)
  }

  def whereQuery(s: WhereQuery[_]):execute[Boolean] = {
    conditionExecute(s.collection.toString, buildCondition(s.w.c))
  }

  def selectQuery[R](s: SelectQuery[R]):execute[R] = {

    def as(a:DBObject, e: Field[_,_]) = e.fromDBObject(a.get(e.fieldName))

    s.s match {
      case e: se => se(s.c.toString, buildCondition(s.s.w), a => s.c.fromDBObject(a).asInstanceOf[R])
      case e: sf1 => sf(s.c.toString, buildCondition(s.s.w), buildSelectFields(s.s), a => as(a, e.c1).asInstanceOf[R])
      case e: sf2 => sf(s.c.toString, buildCondition(s.s.w), buildSelectFields(s.s), a => (as(a, e.c1), as(a, e.c2)).asInstanceOf[R])
      case e: sf3 => sf(s.c.toString, buildCondition(s.s.w), buildSelectFields(s.s), a => (as(a, e.c1), as(a, e.c2), as(a, e.c3)).asInstanceOf[R])
    }
  }

  def selectQueryAsString(s: SelectQuery[_]) = {
    val fields = buildSelectFields(s.s)
    val condition = buildCondition(s.s.w).toString
    val collection = s.c
    s.s match {
      case e: se  => "db.%s.find(%s)".format(collection, condition)
      case _      => "db.%s.find(%s, %s)".format(collection, condition, fields)
    }
  }

  def modifyQuery[F](s: ModifyQuery[_]):execute[Boolean] = {
    updateExecute(s.c.toString, buildCondition(s.s.condition.c), buildModify(s.s))
  }

  def modifyQueryAsString(u: ModifyQuery[_]) = {
    val collection = u.c
    val condition = buildCondition(u.s.condition.c).toString
    val modify = buildModify(u.s)
    "db.%s.update(%s, %s)".format(collection, condition, modify)
  }

  def joinQuery[R](join: JoinQuery[R]):execute[R] = {

    def relation(j: join, dbo: DBObject) = {
      val collection = dbo.get(s"${j.owner.collection}_${j.joined.collection}__${j.joined.fieldName}").asInstanceOf[BasicDBList].toArray.toList
      j match {
        case y: joinOne =>
          collection.headOption.map(y.joined.collection.fromDBObject(_))
        case y: joinMany =>
          collection.map(y.joined.collection.fromDBObject(_))
      }
    }

    val condition = new BasicDBObject("$match", buildCondition(join.joined.condition))
    val joins = buildJoin(join.joined.joinExpression) //TODO

    joinExecute[R](join.collection.toString, condition :: joins.toList, { m =>

      val head = join.collection.fromDBObject(m)

      join.joined match {
        case JoinStateYield1(c, j1) => (head, relation(j1, m)).asInstanceOf[R]
        case JoinStateYield2(c, j1, j2) => (head, relation(j1, m), relation(j2, m)).asInstanceOf[R]
      }

    })
  }

  def joinQueryAsString(join: JoinQuery[_]):String = {
    val dblist = new BasicDBList()
    buildJoin(join.joined.joinExpression).foreach {dblist.add}
    "db.%s.aggregate(%s)".format(join.collection.toString, dblist.toString)
  }

  def buildJoin(joins: Seq[join]):Seq[DBObject] = {
    joins.sortBy(_.owner.toString).reverse.map { join =>
      val builder = BasicDBObjectBuilder.start()
      builder.append("$lookup", BasicDBObjectBuilder.start()
        .append("from", join.joined.collection.toString)
        .append("localField", join.owner.toString)
        .append("foreignField", join.joined.toString)
        .append("as", s"${join.owner.collection}_${join.joined.collection}__${join.joined.fieldName}").get()).get()
    }
  }

  private [record] def buildModify(modify: update):DBObject = {
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

  private [record] def buildListToDBObject(list: List[update]) = {
    val builder = BasicDBObjectBuilder.start()
    list.foreach(a => builder.append(a.left.toString, a.right))
    builder.get()
  }

  private [record] def buildSelectFields[R](select: SelectState[R]) = {
    val builder = BasicDBObjectBuilder.start()
    select match {
      case e: sf1 =>
        builder.append(e.c1.toString, 1)
      case e: sf2 =>
        builder.append(e.c1.toString, 1)
        builder.append(e.c2.toString, 1)
      case e: sf3 =>
        builder.append(e.c1.toString, 1)
        builder.append(e.c2.toString, 1)
        builder.append(e.c3.toString, 1)
      case _ =>
    }
    builder.get()
  }

  private [record] def buildCondition(predicate: Expression[_], builder: BasicDBObjectBuilder = BasicDBObjectBuilder.start()):DBObject = {
    predicate match {
      case b: booleanExpression =>
        b.operator match {
          case "$eq" => builder.append(b.left.toString, b.left.asDBObject(b.right)).get
          case _ => builder.append(
            b.left.toString, new BasicDBObject(b.operator, b.left.asDBObject(b.right))).get
        }
      case l: logicalExpression =>
        builder.append(l.operator, (buildCondition(l.left)::buildCondition(l.right)::Nil).toArray).get
      case allExpression() => new BasicDBObject()
    }
  }

}