import record.signatures._

package object record {

  private [record] type M = MetaTag[_]
  private [record] val setExpression = classOf[SetState[_,_]]
  private [record] val minExpression = classOf[MinState[_]]
  private [record] val maxExpression = classOf[MaxState[_]]
  private [record] val renameExpression = classOf[RenameState[_,_]]
  private [record] val incExpression = classOf[IncState[_,_]]
  private [record] val mulExpression = classOf[MulState[_,_]]
  private [record] val unsetExpression = classOf[UnsetState[_,_]]

  private [record] type booleanExpression = BooleanExpression[_,_, _]
  private [record] type logicalExpression = LogicalExpression[_]
  private [record] type se = SelectEntity[_]
  private [record] type sf1 = SelectFields1[_,_]
  private [record] type sf2 = SelectFields2[_,_, _]
  private [record] type sf3 = SelectFields3[_,_, _, _]
  private [record] type sf4 = SelectFields4[_,_, _, _, _]
  private [record] type sf5 = SelectFields5[_,_, _, _, _, _]
  private [record] type sf6 = SelectFields6[_,_, _, _, _, _, _]
  private [record] type sf7 = SelectFields7[_,_, _, _, _, _, _, _]
  private [record] type sf8 = SelectFields8[_,_, _, _, _, _, _, _, _]
  private [record] type sf9 = SelectFields9[_,_, _, _, _, _, _, _, _, _]
  private [record] type update = ModifyState[_]
  private [record] type join = Join[_,_,_]
  private [record] type joinOne = JoinOne[_,_,_]
  private [record] type joinMany = JoinMany[_,_,_]

  private [record] val se = selectExecute
  private [record] val sf = selectFieldsExecute

}
