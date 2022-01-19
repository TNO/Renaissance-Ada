with Libadalang.Common;                         use Libadalang.Common;

package Assert_AST is

   procedure Assert_Equal_AST (Expected_String, Actual_String : String;
                               Rule : Grammar_Rule;
                               Message : String);

end Assert_AST;
