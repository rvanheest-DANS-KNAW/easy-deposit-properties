/**
 * Copyright (C) 2019 DANS - Data Archiving and Networked Services (info@dans.knaw.nl)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package nl.knaw.dans.easy.properties.app.graphql.types

import java.util.UUID

import nl.knaw.dans.easy.properties.app.graphql.DataContext
import sangria.relay.{ GlobalId, Node, NodeDefinition }
import sangria.schema.Context

trait NodeType {
  this: DepositType
    with DepositorType
    with StateType
    with IngestStepType
    with IdentifierGraphQLType
    with CurationType
    with SpringfieldType
    with ContentTypeGraphQLType =>

  val NodeDefinition(nodeInterface, nodeField, nodesField) = Node.definition((id: GlobalId, ctx: Context[DataContext, Unit]) => {
    if (id.typeName == "Deposit") ctx.ctx.deposits.getDeposit(UUID.fromString(id.id)).map(Option(_)).toTry
    else if (id.typeName == "State") ctx.ctx.deposits.getStateById(id.id).toTry
    else if (id.typeName == "IngestStep") ctx.ctx.deposits.getIngestStepById(id.id).toTry
    else if (id.typeName == "Identifier") ctx.ctx.deposits.getIdentifierById(id.id).toTry
    else if (id.typeName == "Curator") ctx.ctx.deposits.getCurationById(id.id).map(_.map(_.getCurator)).toTry
    else if (id.typeName == "Springfield") ctx.ctx.deposits.getSpringfieldById(id.id).toTry
    else if (id.typeName == "ContentType") ctx.ctx.deposits.getContentTypeById(id.id).toTry
    else None
  }, Node.possibleNodeTypes[DataContext, Node](DepositType, StateType, IngestStepType, IdentifierObjectType, CurationType, SpringfieldType, ContentTypeType))
}
