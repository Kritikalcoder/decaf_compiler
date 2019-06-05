#include <bits/stdc++.h>
#include "constructs.h"
#include <llvm/IR/Value.h>
using namespace llvm;
using namespace std;

union ASTNode{
	int number;
	char* value;
	class Program* prog;
	class Field_decl_list* fields;
	class Field_decl* field_decl;
    class Field_decl_var_list* field_decl_var_list;
    class Field_decl_var* field_decl_var;
    class Method_decl_list* methods;
    class Method_decl* method_decl;
    class Method_arg_list* method_arg_list;
    // class Method_arg* method_arg;
    class Block* block;
    class Variable_decl_list* var_decl_list;
    class Variable_decl* var_decl;
    class Variable_list* var_list;
    class Statement_list* statement_list;
    class Statement* statement;
    class Expression* expr;
    class Location* loc;
    class Literal* lit;
    class Method_call* method_call;
    class Method_call_arg_list* method_call_arg_list;
    // class Method_call_arg* method_call_arg;
    class Callout_arg_list* callout_arg_list;
    class Callout_arg* callout_arg;
    
	ASTNode() = default;
	~ASTNode() = default;
};

typedef union ASTNode YYSTYPE;

#define YYSTYPE_IS_DECLARED 1

class Program{
    private:
        string name;
        class Method_decl_list* methods;
        class Field_decl_list* fields;
    public:
        Program(string, class Field_decl_list*, class Method_decl_list*);
        Constructs* comp_constructs;
        Value* generate_IR();
        void generate_code_dump();
};

///////////////////////////////////////////////////////

class Method_decl_list{
    private:
        vector< class Method_decl* > m_decl_list;
        int count;

    public:
        Method_decl_list();
        void append(class Method_decl*);
        Value* generate_IR(Constructs* );
};

class Method_decl{
    private:
        string name, data_type;
        class Method_arg_list* m_arg_list;
        class Block* m_block;
    public:
        Method_decl(string, string, class Method_arg_list*, class Block*);
        Function* generate_IR(Constructs* );
};

class Method_arg_list {
    private:
        vector <class Method_arg*> m_arg_list;
        int count;
    public:
        Method_arg_list();
        void append(class Method_arg*);
        vector<class Method_arg*> get_arg_list();
};

class Method_arg{
    private:
        string name, data_type;
    public:
        Method_arg(string, string);
        string get_name();
        string get_datatype();

};

///////////////////////////////////////////////////////

class Field_decl_list{
    private:
        vector <class Field_decl*> f_decl_list;
        int count;
        
    public:
        Field_decl_list();
        void append(class Field_decl*);  
        Value* generate_IR(Constructs*);
};

class Field_decl{
    private:
        string data_type;
        int len;
        class Field_decl_var_list* f_var_list;
    public:
        Field_decl(string, class Field_decl_var_list*);
        // class vector<class Field_decl_var*> get_field_var_list();
        Value* generate_IR(Constructs*);
        // int Field_decl::get_length();    
};

class Field_decl_var_list{
    private:
        vector <class Field_decl_var*> f_vars;
        int count;
    public:
        Field_decl_var_list();
        void append(class Field_decl_var*);
        class vector<class Field_decl_var*> get_field_var_list();
};

class Field_decl_var{
    private:
        string decl_type; /* Array Vs normal */
        string var_name, data_type;
        int len; /* length of array */
    public:
        Field_decl_var(string, string);
        Field_decl_var(string, string, int);
        void set_data_type(string);
        bool is_array();
        string get_name();
        int get_length();
        // string get_decl_type(); -- same as is_array()
};

//////////////////////////////////////////////////////////////

class Variable_decl_list{
    private:
        vector <class Variable_decl*> var_decls;
        int count;
    public:
        Variable_decl_list();
        void append(class Variable_decl*);
        Value* generate_IR(map<string, llvm::AllocaInst*>, Constructs*);
};

class Variable_decl{
    private:
        string data_type;
        class Variable_list* var_list;
    public:
        Variable_decl(string, class Variable_list*);
        Value* generate_IR(map<string, llvm::AllocaInst*> &, Constructs*);
};

class Variable_list{
    private:
        int count;
        vector <string> variables;
    public:
        Variable_list(string);
        void append(string);
        vector <string> get_variables();
};

//////////////////////////////////////////////////////////////

class Statement_list{
    private:
        vector <class Statement*> statements;
        int count;
    public:
        Statement_list();
        void append(class Statement*);
        Value* generate_IR(Constructs*);
        bool has_return();
};

class Statement{
    protected:
        string s_type;
    public:
        Statement() {this->s_type = "nonreturn";}
        virtual bool has_return() {return false;}
        virtual Value* generate_IR(Constructs* comp_constructs) {}

};

//////////////////////////////////////////////////////////////

class Assign_statement:public Statement{
    private:
        class Location* loc;
        class Expression* expr;
        string op;
    public:
        Assign_statement(class Location*, string, class Expression*);
        Value* generate_IR(Constructs* ) override;
};


class Block:public Statement{
    private:
        class Variable_decl_list* var_decl_list;
        class Statement_list* statement_list;
    public:
        Block(class Variable_decl_list*, class Statement_list*);
        bool has_return();
        Value* generate_IR(Constructs*) override;
};

class If_statement:public Statement{
    private:
        class Expression* expr;
        class Block* block;
    public:
        If_statement(class Expression*, class Block*);
        Value* generate_IR(Constructs*) override;
        bool has_return() override;

};

class If_else_statement:public Statement{
    private:
        class Expression* expr;
        class Block* if_block;
        class Block* else_block;
    public:
        If_else_statement(class Expression*, class Block*, class Block*);
        Value* generate_IR(Constructs*) override;
        bool has_return() override;

};

class For_statement:public Statement{
    private:
        string id;
        class Expression* start_expr;
        class Expression* end_expr;
        class Block* block;
    public:
        For_statement(string, class Expression*, class Expression*, class Block*);
        Value* generate_IR(Constructs*) override;
};

class Return_statement:public Statement{
    private:
        class Expression* expr;
    public:
        Return_statement();
        Return_statement(class Expression*);
        Value* generate_IR(Constructs*) override;
};

class Break_statement:public Statement{
    public:
        Break_statement();
        Value* generate_IR(Constructs*) override;
};

class Continue_statement:public Statement{
    public:
        Continue_statement();
        Value* generate_IR(Constructs*) override;
};

//////////////////////////////////////////////////////////////

class Expression{
    protected:
        string e_type;
    public:
        Expression() = default;
        virtual Value* generate_IR(Constructs* comp_constructs) {}
        string get_expr_type();
        virtual string to_string() {}
};

class Location: public Expression{
    private:
        string id;
        string loc_type;
        class Expression* expr;
    public:
        Location(string, string);
        Location(string, string, class Expression*);
        string get_var_name();
        Value* generate_IR(Constructs*);
        // 	bool is_array(); /* tells if its array or not */
        // 	class Expr* getExpr();  
};

class Method_call: public Statement, public Expression{
    public:
        Method_call() {}
};

//////////////////////////////////////////////////////////////

class Literal: public Expression{
    protected:
        string l_type;
    // 	literalType ltype; /* Integer bool or char */
    // public:
    // 	virtual void traverse(){}
    // 	virtual int getValue(){}
    // 	virtual string toString(){}
    public:
        Literal();
};

class Int_literal: public Literal {
    private:
        int int_lit;
    public:
        Int_literal(int);
        Value* generate_IR(Constructs*);
        // 	void traverse();
        // 	int getValue();
        // 	string toString();
};

class Char_literal: public Literal {
    private:
        char char_lit;
    public:
        Char_literal(string);
};

class Bool_literal: public Literal {
    private:
        string bool_lit;
    public:
        Bool_literal(string);
        Value* generate_IR(Constructs*);
};

class String_literal: public Literal {
private:
    string str_lit;
public:
    explicit String_literal(string);
    Value* generate_IR(Constructs*) override;
};

//////////////////////////////////////////////////////////////

class Binary_expr: public Expression {
    private:
        class Expression* left_operand;
        string op;
        class Expression* right_operand;
    public:
        Binary_expr(class Expression*, string, class Expression*);
        Value* generate_IR(Constructs* comp_constructs) override;
        
};

class Unary_expr: public Expression {
    private:
        string op;
        class Expression* operand;
    public:
        Unary_expr(string, class Expression*);
        Value* generate_IR(Constructs*);
};

////////////////////////////////////////////////////////////////

class Normal_call:public Method_call{
    private:
        string method_name;
        class Method_call_arg_list* m_arg_list;
    public:
        Normal_call(string, class Method_call_arg_list*);
        Value* generate_IR(Constructs*);
};

class Callout_call:public Method_call{
    private:
        string method_name;
        class Callout_arg_list* c_arg_list;
    public:
        Callout_call(string, class Callout_arg_list*);
        Value* generate_IR(Constructs*);

};

class Method_call_arg_list{
    private:
        vector <class Expression*> m_args;
        int count;
    public:
        Method_call_arg_list();
        void append(class Expression*);
        vector <class Expression*> get_args() {return m_args;}
};

class Callout_arg_list{
    private:
        vector <class Callout_arg*> c_args;
        int count;
    public:
        Callout_arg_list();
        void append(class Callout_arg*);
        vector <class Callout_arg*> get_args() {return this->c_args;}
};

class Callout_arg{
    private:
        string str_type;
        class Expression* expr;
        // string str_arg;
    public:
        Callout_arg(string);
        Callout_arg(class Expression*);
        Value* generate_IR(Constructs*);
};