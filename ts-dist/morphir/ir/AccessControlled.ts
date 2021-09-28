// Generated by morphir-elm

import { Morphir_IR_AccessControlled_Access } from "../../morphir/ir/AccessControlled"

/* Public or private access.
*/
export type Morphir_IR_AccessControlled_Access = Private | Public

interface Morphir_IR_AccessControlled_Private{
  kind: "Private";
}

interface Morphir_IR_AccessControlled_Public{
  kind: "Public";
}

/* Type that represents different access levels.
*/
export type Morphir_IR_AccessControlled_AccessControlled<a> = {
  Access: Morphir_IR_AccessControlled_Access;
  Value: a;
}
