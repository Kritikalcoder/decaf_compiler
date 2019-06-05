#include <bits/stdc++.h>
#include "ast.h"
#include <llvm/Support/raw_ostream.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
using namespace llvm;
using namespace std;

/////////////////////////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////////////////////////

/* Utility Functions */

Value* print_error(string error_str) { cerr << error_str << "\n"; }

Value* create_operator(Value *left, Value* right, string op, Constructs* comp_constructs) {
    Value* v;
    if (op == "*") {
        v = comp_constructs->Builder->CreateMul(left, right, "product");
    } 
    else if (op == "/") {
        v = comp_constructs->Builder->CreateSDiv(left, right, "division");
    }
    else if (op == "-") {
        v = comp_constructs->Builder->CreateSub(left, right, "difference");
    } 
    else if (op == "+") {
        v = comp_constructs->Builder->CreateAdd(left, right, "sum");
    } 
    else if (op == "<=") {
        v = comp_constructs->Builder->CreateICmpSLE(left, right, "less_e");
    } 
    else if (op == ">=") {
        v = comp_constructs->Builder->CreateICmpSGE(left, right, "great_e");
    } 
    else if (op == "<") {
        v = comp_constructs->Builder->CreateICmpSLT(left, right, "less");
    } 
    else if (op == ">") {
        v = comp_constructs->Builder->CreateICmpSGT(left, right, "great");
    }
    else if (op == "%") {
        v = comp_constructs->Builder->CreateSRem(left, right, "mod_op");
    } 
    else if (op == "!=") {
        v = comp_constructs->Builder->CreateICmpNE(left, right, "not_equal");
    }
    else if (op == "==") {
        v = comp_constructs->Builder->CreateICmpEQ(left, right, "equal_comp");
    } 
    
    return v;
}

/////////////////////////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////////////////////////

/* Constructors */

Program::Program(string name, class Field_decl_list* fields, class Method_decl_list* methods) {
    this->name = move(name);
    this->methods = methods;
    this->fields = fields;
    this->comp_constructs = new Constructs(name);
}

Method_decl_list::Method_decl_list() { this->count = 0; }

Method_decl::Method_decl(string data_type, string name, class Method_arg_list* m_arg_list, class Block* blk) {
    this->data_type = move(data_type);
    this->name = move(name);
    this->m_arg_list = m_arg_list;
    this->m_block = blk;
}

Method_arg_list::Method_arg_list() { this->count = 0; }
void Method_arg_list::append(class Method_arg* m_arg) {
    this->m_arg_list.push_back(m_arg);
    this->count += 1;
}

Method_arg::Method_arg(string data_type, string name) {
    this->data_type = move(data_type);
    this->name = move(name);
}

Block::Block(class Variable_decl_list* vdl, class Statement_list* sl) {
    this->var_decl_list = vdl;
    this->statement_list = sl;
}

Field_decl_list::Field_decl_list() { this->count = 0; }

Field_decl::Field_decl(string data_type, class Field_decl_var_list* fvl) {
    this->data_type = move(data_type);
    this->f_var_list = fvl;
    // cout << "Data type: " << this->data_type << "\n";
    for(int i = 0; i < f_var_list->get_field_var_list().size(); i++) {
        f_var_list->get_field_var_list()[i]->set_data_type(data_type);
    }
}

Field_decl_var_list::Field_decl_var_list() {
    this->count += 1;
}

Field_decl_var::Field_decl_var(string name, string decl_type) {
    this->decl_type = move(decl_type);
    this->var_name = move(name);
    this->len = 1;
}

Field_decl_var::Field_decl_var(string name, string decl_type, int len) {
    this->decl_type = move(decl_type);
    this->var_name = move(name);
    this->len = len;
}

Variable_decl_list::Variable_decl_list() {
    this->count = 0;
}

Variable_decl::Variable_decl(string type, class Variable_list* var_list) {
    this->data_type = move(type);
    this->var_list = var_list;
}

Variable_list::Variable_list(string id) {
    this->variables.push_back(id);
    this->count = 1;
    // cout << "Variable list\n";
}

Statement_list::Statement_list() {
    // cout << "Statement list\n";
    this->count = 0;
}

If_else_statement::If_else_statement(class Expression* expr, class Block* if_block, class Block* else_block) {
    this->expr = expr;
    this->if_block = if_block;
    this->else_block = else_block;
    this->s_type = "nonreturn";
}


Assign_statement::Assign_statement(class Location* loc, string op, class Expression* expr) {
    this->loc = loc;
    this->op = move(op);
    this->expr = expr;
    this->s_type = "nonreturn";
    // cout << "Assignment Statement" << op << "\n";
}

For_statement::For_statement(string id, class Expression* start, class Expression* end, class Block* block) {
    this->id = move(id);
    this->start_expr = start;
    this->end_expr = end;
    this->block = block;
    this->s_type = "nonreturn";
}

Return_statement::Return_statement() {
    // cout << "Empty Return Statement\n";
    this->s_type = "return";
    this->expr = nullptr;
}

Return_statement::Return_statement(class Expression* expr) {
    this->s_type = "return";
    this->expr = expr;
    // cout << "Return Statement\n";
}

If_statement::If_statement(class Expression* expr, class Block* block) {
    this->expr = expr;
    this->block = block;
    this->s_type = "nonreturn";
}

Break_statement::Break_statement() {
    // cout << "Break Statement\n";
    this->s_type = "nonreturn";
}

Continue_statement::Continue_statement() {
    // cout << "Continue Statement\n";
    this->s_type = "nonreturn";
}


Location::Location(string id, string loc_type) {
    // normal
    this->id = move(id);
    this->loc_type = move(loc_type);
    this->e_type = "location";
    this->expr = nullptr;
}

Location::Location(string id, string loc_type, class Expression* expr) {
    // array
    this->id = move(id);
    this->loc_type = move(loc_type);
    this->expr = expr;
    this->e_type = "location";
}

string Location::get_var_name() {
    return this->id;
}

Literal::Literal() {
    this->e_type = "literal";
}

Int_literal::Int_literal(int int_lit) {
    this->int_lit = int_lit;
    this->l_type = "int";
}


Char_literal::Char_literal(string char_lit) {
    this->char_lit = char_lit[1];
    this->l_type = "char";
}

Bool_literal::Bool_literal(string bool_lit) {
    this->bool_lit = move(bool_lit);
    this->l_type = "bool";
}

String_literal::String_literal(string str) {
    str = str.substr(1, str.length() - 2);
    this->str_lit = str;
    this->l_type = "string";
}

Binary_expr::Binary_expr(class Expression* left, string op, class Expression* right) {
    this->left_operand = left;
    this->op = move(op);
    this->right_operand = right;
    this->e_type = "binary";
}

Unary_expr::Unary_expr(string op, class Expression* expr) {
    this->op = move(op);
    this->operand = expr;
    this->e_type = "unary";
}

Normal_call::Normal_call(string name, class Method_call_arg_list* arg_list) {
    this->method_name = move(name);
    this->m_arg_list = arg_list;
}

Callout_call::Callout_call(string name, class Callout_arg_list* arg_list) {
    name = name.substr(1, name.length() - 2);
    this->method_name = name;
    this->c_arg_list = arg_list;
}

Method_call_arg_list::Method_call_arg_list() {
    this->count = 0;
}

Callout_arg_list::Callout_arg_list() {
    this->count = 0;
}

Callout_arg::Callout_arg(string s_arg) {
    class String_literal* tmp = new String_literal(s_arg);
    this->expr = tmp;
    this->str_type = string("var");
}

Callout_arg::Callout_arg(class Expression* expr) {
    this->str_type = string("expr");
    this->expr = expr;
}

/////////////////////////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////////////////////////

 /* Helper Functions */

void Program::generate_code_dump() {
    cerr << "Generating LLVM IR\n";
    this->comp_constructs->TheModule->print(llvm:: outs(), nullptr);
}

void Method_decl_list::append(class Method_decl* m_decl) {
    m_decl_list.insert(m_decl_list.begin(), m_decl);
    this->count += 1;
}

vector<class Method_arg*> Method_arg_list::get_arg_list() {
    return this->m_arg_list;
}

string Method_arg::get_name() {
    return this->name;
}

string Method_arg::get_datatype() {
    return this->data_type;
}

bool Block::has_return() {
    return this->statement_list->has_return();
}

void Field_decl_list::append(class Field_decl* f_decl) {
    this->f_decl_list.push_back(f_decl);
    this->count += 1;
}

void Field_decl_var_list::append(class Field_decl_var* f_var) {
    this->f_vars.push_back(f_var);
    this->count += 1;
}

class vector<class Field_decl_var*> Field_decl_var_list::get_field_var_list() {
    return this->f_vars;
}

void Field_decl_var::set_data_type(string data_type) {
    this->data_type = move(data_type);
}

bool Field_decl_var::is_array() {
    return (this->decl_type == "array");
}

string Field_decl_var::get_name() {
    return this->var_name;
}

int Field_decl_var::get_length() {
    return this->len;
}

void Variable_decl_list::append(class Variable_decl* var_decl) {
    this->var_decls.push_back(var_decl);
    this->count += 1;
}

void Variable_list::append(string id) {
    this->variables.push_back(id);
    this->count += 1;
    // cout << "Variable list count: " << this->count << "\n";
}

vector <string> Variable_list::get_variables() {
    return this->variables;
}

bool Statement_list::has_return() {
    
    for (int i = 0; i < statements.size(); i++) {
        if (statements[i]->has_return()) {
            return true;
        }
    }
    return false;
}

void Statement_list::append(class Statement* stmnt) {
    this->statements.push_back(stmnt);
    this->count += 1;
    // cout << "Statement list count: " << this->count << "\n";
}

bool If_else_statement::has_return() {
    bool status = false;
    status = status || this->if_block->has_return();
    status = status || this->else_block->has_return();
    return status;
}

bool If_statement::has_return() {
    return this->block->has_return();
}

string Expression::get_expr_type() {
    return this->e_type;
}

void Method_call_arg_list::append(class Expression* expr) {
    this->m_args.push_back(expr);
    this->count += 1;
}

void Callout_arg_list::append(class Callout_arg* c_arg) {
    c_args.insert(c_args.begin(), c_arg);
    this->count += 1;
}

/////////////////////////////////////////////////////////////////////////////////////////////////

 /* IR Generation Functions */

Value* Program::generate_IR() {
    Value* V;
    V = fields->generate_IR(this->comp_constructs);
    if (V == nullptr) {
        print_error("Invalid field Declarations");
        return nullptr;
    }
    V = methods->generate_IR(this->comp_constructs);
    if (V == nullptr) {
        print_error("Invalid method Declarations");
        return nullptr;
    }
    // cerr << "Program done\n";
    return V;
}

Value* Method_decl_list :: generate_IR(Constructs* comp_constructs) { 
    for (auto &i : m_decl_list) {
        i->generate_IR(comp_constructs);
    }
    Value *v;
    v = ConstantInt::get(comp_constructs->Context, APInt(32, 1));
    // cerr << "Method decl list done\n";
    return v;
}

Function* Method_decl::generate_IR(Constructs* comp_constructs) {

    vector <string> method_arg_names;
    vector <string> method_arg_types;
    vector <class Method_arg*> m_args;
    m_args = m_arg_list->get_arg_list();

    vector <Type*> arguments;

    auto arg_size = m_args.size();
    for (auto &arg : m_args) {
        string arg_type;
        arg_type = arg->get_datatype();
        string arg_name;
        arg_name = arg->get_name();

        if(arg_type == "int") {
            arguments.push_back(Type::getInt32Ty(comp_constructs->Context));
        }
        else if (arg_type == "boolean") {
            arguments.push_back(Type::getInt1Ty(comp_constructs->Context));
        }
        else {
            comp_constructs->error_count++;
            print_error("Argument type can be int or bool");
            return nullptr;
        }
        method_arg_types.emplace_back(arg_type);
        method_arg_names.emplace_back(arg_name);
    }

    Type* return_type;
    if (data_type == "int") {
        return_type = Type::getInt32Ty(comp_constructs->Context);
    }
    else if (data_type == "boolean") {
        return_type = Type::getInt1Ty(comp_constructs->Context);
    }
    else if (data_type == "void") {
        return_type = Type::getVoidTy(comp_constructs->Context);
    }
    else {
        comp_constructs->error_count++;
        print_error("Invalid return type for " + name + " - not int, boolean or void.");
        return nullptr;
    }

    FunctionType* FType;
    FType = llvm::FunctionType::get(return_type, arguments, false);
    Function* F;
    F = llvm::Function::Create(FType, Function::ExternalLinkage, name, comp_constructs->TheModule);

    unsigned it;
    it = 0;
    for (Function::arg_iterator AI = F->arg_begin(); it != arg_size; ++AI, ++it) {
        AI->setName(method_arg_names[it]);
    }

    BasicBlock* BBlock;
    BBlock = BasicBlock::Create(comp_constructs->Context, "entry", F);
    comp_constructs->Builder->SetInsertPoint(BBlock);

    it = 0;
    for (auto &Arg : F->args()) {
        AllocaInst *Alloca;
        Alloca = comp_constructs->CreateEntryBlockAlloca(F, method_arg_names[it], method_arg_types[it]);
        comp_constructs->Builder->CreateStore(&Arg, Alloca);
        comp_constructs->NamedValues[method_arg_names[it]] = Alloca;
        it++;
    }
    
    Value *return_val;
    return_val = m_block->generate_IR(comp_constructs);
    if (return_val) {
        if (data_type != "void")
            comp_constructs->Builder->CreateRet(return_val);
        else
            comp_constructs->Builder->CreateRetVoid();

        verifyFunction(*F);
        comp_constructs->TheFPM->run(*F);
        // cerr << "Method decl done\n";
        return F;
    }
    // cerr << "Method decl done\n";
    F->eraseFromParent();
    return nullptr;
}

Value* Block::generate_IR(Constructs* comp_constructs) {
    Value* V;
    map<string, llvm::AllocaInst *> Old_vals;
    V = var_decl_list->generate_IR(Old_vals, comp_constructs);
    if (V == nullptr) {
        return V;
    }
    V = statement_list->generate_IR(comp_constructs);
    auto it = Old_vals.begin();
    for (; it != Old_vals.end(); it++) {
        comp_constructs->NamedValues[it->first] = Old_vals[it->first];
    }
    // cerr << "Block done\n";
    return V;
}

Value* Field_decl_list :: generate_IR(Constructs* comp_constructs) { 
    for (auto &i : f_decl_list) { i->generate_IR(comp_constructs); }
    Value* v;
    v = ConstantInt::get(comp_constructs->Context, APInt(32, 1));
    // cerr << "Field decl list done\n";
    return v;
}


Value* Field_decl::generate_IR(Constructs* comp_constructs) {
    Value *V;
    llvm::Type *ty;
    ty = nullptr;
    /* Get the type reference */
    if (data_type == "int") {
        ty = Type::getInt32Ty(comp_constructs->Context);
    } else if (data_type == "boolean") {
        ty = Type::getInt1Ty(comp_constructs->Context);
    }

    for (auto var : f_var_list->get_field_var_list()) {
        /* Allocate one location of global variable for all */
        if (var->is_array()) {
            ArrayType *arrType;
            arrType = ArrayType::get(ty, var->get_length());
            GlobalVariable *gv;
            gv = new GlobalVariable(*(comp_constructs->TheModule), arrType, false,
                                                    GlobalValue::ExternalLinkage, nullptr,
                                                    var->get_name());
            gv->setInitializer(ConstantAggregateZero::get(arrType));
        } 
        
        else {
            GlobalVariable *gv = new GlobalVariable(*(comp_constructs->TheModule), ty, false,
                                                    GlobalValue::ExternalLinkage, nullptr,
                                                    var->get_name());
            gv->setInitializer(Constant::getNullValue(ty));
        }
    }

    V = ConstantInt::get(comp_constructs->Context, APInt(32, 1));
    // cerr << "Field decl done\n";
    return V;
}

Value* Variable_decl_list::generate_IR(map<string, llvm::AllocaInst*> vals_map, Constructs* comp_constructs) {
    Value* V;
    V = ConstantInt::get(comp_constructs->Context, APInt(32, 1));
    for (auto &decl : var_decls) {
        V = decl->generate_IR(vals_map, comp_constructs);
        if (V == nullptr) {
            return V;
        }
    }
    // cerr << "Variable decl list done\n";
    return V;
}

Value* Variable_decl::generate_IR(map<string, llvm::AllocaInst*> &Old_vals, Constructs* comp_constructs) {
    llvm::Function *TheFunction;
    TheFunction = comp_constructs->Builder->GetInsertBlock()->getParent();
    vector <string> vars;
    vars = this->var_list->get_variables();

    for (int i = 0; i < vars.size(); i++) {
        string var;
        var = vars[i];
        Value *initval;
        initval = nullptr;
        AllocaInst *Alloca;
        Alloca = nullptr;

        if (this->data_type == "int") {
            initval = ConstantInt::get(comp_constructs->Context, APInt(32, 0));
            Alloca = comp_constructs->CreateEntryBlockAlloca(TheFunction, var, "int");
        } else if (this->data_type == "boolean") {
            initval = ConstantInt::get(comp_constructs->Context, APInt(1, 0));
            Alloca = comp_constructs->CreateEntryBlockAlloca(TheFunction, var, "boolean");
        }
        comp_constructs->Builder->CreateStore(initval, Alloca);
        /* Store the old value to old_vals and new value to named values */
        Old_vals[var] = comp_constructs->NamedValues[var];
        comp_constructs->NamedValues[var] = Alloca;
    }
    Value *v;
    v = ConstantInt::get(comp_constructs->Context, APInt(32, 1));
    // cerr << "Variable decl done\n";
    return v;
}

Value* Statement_list::generate_IR(Constructs* comp_constructs) {
    Value* V;
    V = ConstantInt::get(comp_constructs->Context, llvm::APInt(32, 1));
    for (auto &stmt : statements) {
        V = stmt->generate_IR(comp_constructs);
    }
    // cerr << "Statement done\n";
    return V;
}

Value* If_else_statement::generate_IR(Constructs* comp_constructs) {
    Value *cond;
    cond = this->expr->generate_IR(comp_constructs);

    if (cond == nullptr) {
        
        return print_error("Invalid Expr: IF else block");
        comp_constructs->error_count++;
    }

    Function *TheFunction;
    TheFunction = comp_constructs->Builder->GetInsertBlock()->getParent();

    BasicBlock* if_block;
    BasicBlock* else_block;
    BasicBlock* next_block;
    BasicBlock* other_block;

    if_block = BasicBlock::Create(comp_constructs->Context, "if", TheFunction);
    else_block = BasicBlock::Create(comp_constructs->Context, "else");
    next_block = BasicBlock::Create(comp_constructs->Context, "ifcont");
    other_block = else_block;

    bool return_if, return_else;

    return_else = false;
    return_if = this->if_block->has_return();
    
    if (this->else_block == nullptr) {
        other_block = next_block;
    }

    comp_constructs->Builder->CreateCondBr(cond, if_block, other_block);
    comp_constructs->Builder->SetInsertPoint(if_block);
    
    Value *if_val;
    if_val = this->if_block->generate_IR(comp_constructs);
    if (if_val == nullptr) { return nullptr; }

    if (!return_if) {
        comp_constructs->Builder->CreateBr(next_block);
    }

    if_block = comp_constructs->Builder->GetInsertBlock();
    
    // Create insert point for else block
    Value *else_val;
    else_val = nullptr;

    if (this->else_block != nullptr) {
        TheFunction->getBasicBlockList().push_back(else_block);
        comp_constructs->Builder->SetInsertPoint(else_block);
        else_val = this->else_block->generate_IR(comp_constructs);
        if (else_val == nullptr) {
            return nullptr;
        }
        return_else = this->else_block->has_return();
        if (!return_else)
            comp_constructs->Builder->CreateBr(next_block);
    }

    TheFunction->getBasicBlockList().push_back(next_block);
    comp_constructs->Builder->SetInsertPoint(next_block);

    if (return_else && return_if) {
        Type *retType;
        retType = comp_constructs->Builder->GetInsertBlock()->getParent()->getReturnType();

        if (retType == Type::getVoidTy(comp_constructs->Context))
            comp_constructs->Builder->CreateRetVoid();
        else {
            comp_constructs->Builder->CreateRet(ConstantInt::get(comp_constructs->Context, APInt(32, 0)));
        }
    }

    Value *v;
    v = ConstantInt::get(comp_constructs->Context, APInt(32, 0));
    return v;    
}

Value* Assign_statement::generate_IR(Constructs* comp_constructs) {
    Value *cur;
    cur = comp_constructs->NamedValues[loc->get_var_name()];
    if (cur == nullptr) {
        cur = comp_constructs->TheModule->getGlobalVariable(loc->get_var_name());
    }
    if (cur == nullptr) {
        comp_constructs->error_count++;
        return print_error("Unknown Variable Name " + loc->get_var_name());
    }
    Value *val;
    val = this->expr->generate_IR(comp_constructs);
    if (this->expr->get_expr_type() == "location") {
        val = comp_constructs->Builder->CreateLoad(val);
    }
    Value *lhs;
    lhs = this->loc->generate_IR(comp_constructs);
    cur = comp_constructs->Builder->CreateLoad(lhs);
    if (val == nullptr) {
        comp_constructs->error_count++;
        return print_error("Error in right hand side of the Assignment");
    }

    if (this->op == "+=") {
        val = comp_constructs->Builder->CreateAdd(cur, val, "addEqualToTmp");
    } 
    else if (this->op == "-=") {
        val = comp_constructs->Builder->CreateSub(cur, val, "subEqualToTmp");
    }
    // cerr << "Assign statement done\n";
    return comp_constructs->Builder->CreateStore(val, lhs);
}

Value* Return_statement::generate_IR(Constructs *comp_constructs) {
    Value *V;
    V = nullptr;

    if (this->expr != nullptr) {
        /// Generate IR for ret_expr
        V = this->expr->generate_IR(comp_constructs);
        if (this->expr->get_expr_type() == "location") {
            V = comp_constructs->Builder->CreateLoad(V);
        }
        comp_constructs->Builder->CreateRet(V);
        // cerr << "Return statement done\n";
        return V;
    }
    else {
        comp_constructs->Builder->CreateRetVoid();
        // cerr << "Return statement done\n";
        return V;
    }
}

Value* For_statement::generate_IR(Constructs* comp_constructs) {
    Value *start;
    start = this->start_expr->generate_IR(comp_constructs);

    if (start == nullptr) { return nullptr; }
    if (this->start_expr->get_expr_type() == "location") {
        start = comp_constructs->Builder->CreateLoad(start);
    }

    Function *TheFunction;
    TheFunction = comp_constructs->Builder->GetInsertBlock()->getParent();
    
    AllocaInst *Alloca;
    Alloca = comp_constructs->CreateEntryBlockAlloca(TheFunction, id, string("int"));
    comp_constructs->Builder->CreateStore(start, Alloca);

    Value *value_of_step;
    value_of_step = ConstantInt::get(comp_constructs->Context, APInt(32, 1));
    BasicBlock *pre_header_basic_block;
    pre_header_basic_block = comp_constructs->Builder->GetInsertBlock();

    BasicBlock *body_of_loop;
    body_of_loop = BasicBlock::Create(comp_constructs->Context, "loop", TheFunction);
    BasicBlock *after_basic_block;
    after_basic_block = BasicBlock::Create(comp_constructs->Context, "afterloop", TheFunction);
    
    comp_constructs->Builder->CreateBr(body_of_loop);
    comp_constructs->Builder->SetInsertPoint(body_of_loop);

    PHINode *Variable;
    Variable = comp_constructs->Builder->CreatePHI(Type::getInt32Ty(comp_constructs->Context), 2, id);
    Variable->addIncoming(start, pre_header_basic_block);
    
    Value *cond;
    cond = this->end_expr->generate_IR(comp_constructs);
    if (cond == nullptr) {
        comp_constructs->error_count++;
        return print_error("Invalid Condition");
    }

    if (this->end_expr->get_expr_type() == "location") {
        cond = comp_constructs->Builder->CreateLoad(cond);
    }

    comp_constructs->loops->push(new loopInfo(after_basic_block, body_of_loop, cond, id, Variable));
    AllocaInst *old_val;
    old_val = comp_constructs->NamedValues[this->id];
    comp_constructs->NamedValues[this->id] = Alloca;
    
    if (this->block->generate_IR(comp_constructs) == nullptr) { return nullptr; }

    Value *cur;
    cur = comp_constructs->Builder->CreateLoad(Alloca, this->id);

    Value *next_val;
    next_val = comp_constructs->Builder->CreateAdd(cur, value_of_step, "NextVal");
    comp_constructs->Builder->CreateStore(next_val, Alloca);

    cond = comp_constructs->Builder->CreateICmpSLT(next_val, cond, "loopcondition");

    BasicBlock *loopEndBlock;
    loopEndBlock = comp_constructs->Builder->GetInsertBlock();

    comp_constructs->Builder->CreateCondBr(cond, body_of_loop, after_basic_block);
    comp_constructs->Builder->SetInsertPoint(after_basic_block);
    Variable->addIncoming(next_val, loopEndBlock);

    if (old_val) {
        comp_constructs->NamedValues[this->id] = old_val;
    } 
    else {
        comp_constructs->NamedValues.erase(this->id);
    }
    Value *V;
    V = ConstantInt :: get(comp_constructs->Context, APInt(32, 1));
    // cerr << "For statement done\n";
    return V;
}

Value* If_statement::generate_IR(Constructs* comp_constructs) {
    /* if condition */
    Value* cond = this->expr->generate_IR(comp_constructs);
    if (cond == nullptr) {
        comp_constructs->error_count++;
        return print_error("Invalid Expression in the IF");
    }
    Function *TheFunction = comp_constructs->Builder->GetInsertBlock()->getParent();
    BasicBlock *if_block = BasicBlock::Create(comp_constructs->Context, "if", TheFunction);
    BasicBlock *next_block = BasicBlock::Create(comp_constructs->Context, "ifcont");
    BasicBlock *other_block = next_block;
    bool return_if = this->block->has_return();
    
    comp_constructs->Builder->CreateCondBr(cond, if_block, other_block);
    comp_constructs->Builder->SetInsertPoint(if_block);
    
    Value* if_val = this->block->generate_IR(comp_constructs);
    if (if_val == nullptr) {
        return nullptr;
    }
    if (!return_if) {
        comp_constructs->Builder->CreateBr(next_block);
    }

    if_block = comp_constructs->Builder->GetInsertBlock();
    
    TheFunction->getBasicBlockList().push_back(next_block);
    comp_constructs->Builder->SetInsertPoint(next_block);
    Value *V = ConstantInt::get(comp_constructs->Context, APInt(32, 0));
    // cerr << "If statement done\n";
    return V;
}

Value* Break_statement::generate_IR(Constructs *comp_constructs) {
    loopInfo *currentLoop;
    currentLoop = comp_constructs->loops->top();
    comp_constructs->Builder->CreateBr(currentLoop->getAfterBlock());
    Value *V;
    V = ConstantInt :: get(comp_constructs->Context, llvm::APInt(32, 1));
    // cerr << "Break statement done\n";
    return V;
}

Value* Continue_statement::generate_IR(Constructs *comp_constructs) {
    Value *V;
    V = ConstantInt :: get(comp_constructs->Context, llvm::APInt(32, 1));
    loopInfo *currentLoop;
    currentLoop = comp_constructs->loops->top();
    class Expression* condition;
    condition = nullptr;
    string var;
    var = currentLoop->getLoopVariable();
    AllocaInst *Alloca;
    Alloca = comp_constructs->NamedValues[var];
    Value *value_of_step;
    value_of_step = ConstantInt :: get(comp_constructs->Context, APInt(32, 1));
    Value *cur;
    cur = comp_constructs->Builder->CreateLoad(Alloca, var);
    Value *next_val;
    next_val = comp_constructs->Builder->CreateAdd(cur, value_of_step, "NextVal");
    comp_constructs->Builder->CreateStore(next_val, Alloca);
    Value *cond;
    cond = comp_constructs->Builder->CreateICmpULE(next_val, currentLoop->getCondition(), "loopcondition");
    BasicBlock *loopEndBlock;
    loopEndBlock = comp_constructs->Builder->GetInsertBlock();
    comp_constructs->Builder->CreateCondBr(cond, currentLoop->getCheckBlock(), currentLoop->getAfterBlock());
    // cerr << "Continue statement done\n";
    return V;
}

Value* Location::generate_IR(Constructs* comp_constructs) {
    Value *V;
    V = comp_constructs->NamedValues[this->id];

    if (V == nullptr) {
        V = comp_constructs->TheModule->getNamedGlobal(this->id);
    }
    if (V == nullptr) {
        comp_constructs->error_count++;
        return print_error("Unknown Variable name " + this->id);
    }
    if (this->loc_type == "var") {
        return V;
    }
    
    if (this->expr == nullptr) {
        return nullptr;
    }
    Value *index = this->expr->generate_IR(comp_constructs);
    if (this->expr->get_expr_type() == "location") {
        index = comp_constructs->Builder->CreateLoad(index);
    }
    if (index == nullptr) {
        return nullptr;
    }
    vector <Value *> array_index;
    array_index.push_back(comp_constructs->Builder->getInt32(0));
    array_index.push_back(index);
    V = comp_constructs->Builder->CreateGEP(V, array_index, id + "_Index");
    return V;

}

Value* Int_literal::generate_IR(Constructs *comp_constructs) {
    Value *v = ConstantInt::get(comp_constructs->Context, APInt(32, static_cast<uint64_t>(this->int_lit)));
    // cerr << "Int literal done\n";
    return v;
}

Value* Bool_literal::generate_IR(Constructs *comp_constructs) {
    bool val;
    if (this->bool_lit == "true") val = true;
    else if (this->bool_lit == "false") val = false;
    else {
        comp_constructs->error_count++;
        return print_error("Invalid Boolean Literal " + this->bool_lit);
    }
    Value *v = ConstantInt :: get(comp_constructs->Context, APInt(1, val));
    // cerr << "Bool literal done\n";
    return v;
}

Value* String_literal::generate_IR(Constructs* comp_constructs) {
    return comp_constructs->Builder->CreateGlobalStringPtr(this->str_lit);
}

Value* Binary_expr::generate_IR(Constructs* comp_constructs) {
    Value *left = this->left_operand->generate_IR(comp_constructs);
    Value *right = this->right_operand->generate_IR(comp_constructs);

    if (this->left_operand->get_expr_type() == "location") {
        left = comp_constructs->Builder->CreateLoad(left);
    }
    if (this->right_operand->get_expr_type() == "location") {
        right = comp_constructs->Builder->CreateLoad(right);
    }
    if (left == 0) {
        comp_constructs->error_count++;
        return print_error("Error in left operand of " + op);
    } 
    else if (right == 0) {
        comp_constructs->error_count++;
        return print_error("Error in right operand of " + op);
    }
    return create_operator(left, right, this->op, comp_constructs);
}

Value* Unary_expr::generate_IR(Constructs *comp_constructs) {
    Value *v = this->operand->generate_IR(comp_constructs);
    if (this->operand->get_expr_type() == "location") {
        v = comp_constructs->Builder->CreateLoad(v);
    }
    if (this->op == "!") {
        return comp_constructs->Builder->CreateNeg(v, "not_op");
    } 
    else if (this->op == "-") {
        return comp_constructs->Builder->CreateNot(v, "negative");
    }
}

Value* Callout_call::generate_IR(Constructs* comp_constructs) {
    vector <Type*> argTypes;
    vector <Value*> Args;
    vector <class Callout_arg*> args_list = this->c_arg_list->get_args();
    
    for (auto &arg : args_list) {
        Value *tmp = arg->generate_IR(comp_constructs);
        if (tmp == nullptr) {
            return nullptr;
        }
        Args.push_back(tmp);
        argTypes.push_back(tmp->getType());
    }

    ArrayRef <Type *> argsRef(argTypes);
    ArrayRef <Value *> funcargs(Args);
    FunctionType *FType = FunctionType :: get(Type :: getInt32Ty(comp_constructs->Context), argsRef, false);
    Constant *func = comp_constructs->TheModule->getOrInsertFunction(this->method_name, FType);
    if (!func) {
        return print_error("Unknown Function name " + method_name);
    }

    Value *v = comp_constructs->Builder->CreateCall(func, funcargs);
    return v;
}

Value* Callout_arg::generate_IR(Constructs *comp_constructs) {
    if (this->expr == nullptr) {
        comp_constructs->error_count++;
        return print_error("Invalid Callout Arg");
    }
    Value *v = this->expr->generate_IR(comp_constructs);
    if (expr->get_expr_type() == "location") {
        v = comp_constructs->Builder->CreateLoad(v);
    }
    // cerr << "Callout arg done\n";
    return v;
}

Value* Normal_call::generate_IR(Constructs *comp_constructs) {
    Function *callee = comp_constructs->TheModule->getFunction(this->method_name);
    if (callee == nullptr) {
        comp_constructs->error_count++;
        return print_error("Unknown Function name" + this->method_name);
    }
    vector <class Expression *> args_list = this->m_arg_list->get_args();
    if (callee->arg_size() != args_list.size()) {
        comp_constructs->error_count++;
        return print_error("Wrong Number of Parameters Passed");
    }
    vector <Value *> args;
    for (auto &arg : args_list) {
        Value *argVal = arg->generate_IR(comp_constructs);
        if (arg->get_expr_type() == "location") {
            argVal = comp_constructs->Builder->CreateLoad(argVal);
        }
        if (argVal == nullptr) {
            comp_constructs->error_count++;
            print_error("Argument not valid");
            return nullptr;
        }
        args.push_back(argVal);
    }
    reverse(args.begin(), args.end());
    Value *v = comp_constructs->Builder->CreateCall(callee, args);
    // cerr << "normal call done" << endl;
    return v;
}