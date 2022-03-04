with Extraction.Node_Edge_Types;
with Extraction.Utilities;

package body Extraction.Bodies_For_Entries is

   use type LALCO.Ada_Node_Kind_Type;

   procedure Extract_Edges
     (Node : LAL.Ada_Node'Class; Graph : Graph_Operations.Graph_Context)
   is
   begin
      if Node.Kind = LALCO.Ada_Entry_Body then
         declare
            Entry_Body : constant LAL.Entry_Body := Node.As_Entry_Body;
            Entry_Decl : constant LAL.Basic_Decl := Entry_Body.P_Decl_Part;
         begin
            Graph.Write_Edge
              (Entry_Decl, Entry_Body,
               Node_Edge_Types.Edge_Type_Is_Implemented_By);
         end;
      elsif Node.Kind in LALCO.Ada_Accept_Stmt_Range then
         declare
            Accept_Stmt : constant LAL.Accept_Stmt := Node.As_Accept_Stmt;
            Task_Body   : constant LAL.Basic_Decl  :=
              Utilities.Get_Parent_Basic_Decl (Accept_Stmt);
            Entry_Decl : constant LAL.Basic_Decl :=
              Utilities.Get_Referenced_Decl (Accept_Stmt.F_Name);
         begin
            Graph.Write_Edge
              (Entry_Decl, Task_Body,
               Node_Edge_Types.Edge_Type_Is_Implemented_By);
         end;
      end if;
   end Extract_Edges;

end Extraction.Bodies_For_Entries;
