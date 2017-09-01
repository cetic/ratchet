package be.cetic.ratchet.transformer

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import be.cetic.ratchet.TypeMgr
import be.cetic.ratchet.reader.ast.{VarDecl, _}
import be.cetic.ratchet.reader.helpers.{Canvas, CanvasLine, Macro}


/**
  *
  * @author : dimitri durieux <dimitri.durieux@cetic.be>
  */
case class HardeningCanvas(
                            model: String,
                            vars: List[VarDecl], mgr: CheckpointManager)
  extends Canvas(
    model, vars,
    List(
      Macro("SIED".r, List(mgr), 0, (args: List[Any]) =>
        new CanvasLine() {
          var mgr = args.head.asInstanceOf[CheckpointManager]

          override def replace: Instruction
          = mgr.createSiedCheckPoint

          override def children: List[AstNode] = Nil
        }
      ),
      Macro("ERROR\\s*\\((.*)\\)".r, List(mgr), 2, (args: List[Any]) =>
        new CanvasLine() with ASTList[Expression] {
          var mgr = args.head.asInstanceOf[CheckpointManager]
          var left: Expression = args(1).asInstanceOf[Expression]
          var right: Expression = args(2).asInstanceOf[Expression]

          left.father = this
          right.father = this

          override def replace: Instruction = mgr.createErrorCheckPoint(left.clone, right.clone)

          override def list_=(a: List[Expression]): Unit = {
            left = a(0)
            right = a(1)
            left.father = this
            right.father = this
          }

          override def list: List[Expression] = List(left, right)
        }
      )
    )
  ) {

  override def temporaryType = TypeMgr.TMP_TYPE

}


/**
  *
  * @author : dimitri durieux <dimitri.durieux@cetic.be>
  */
case class PrehandlingCanvas(
                              model: String,
                              vars: List[VarDecl])
  extends Canvas(model, vars, Nil) {
  override def temporaryType = TypeMgr.TMP_TYPE
}
