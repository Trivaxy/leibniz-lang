use std::collections::{HashMap, HashSet};

use linked_hash_map::LinkedHashMap;
use parser::{Operator, ParserNode, TypeConstraint};

use crate::{
    function::{Function, NativeFunction},
    instruction::Instruction,
    optimizations::Optimization,
    runtime::LeibnizRuntime,
};

pub struct CodeGen {
    function_table: HashMap<String, Function>,
    function_index_table: HashMap<String, usize>,
    deferred_functions: Vec<(String, Vec<String>, ParserNode)>,
    deferred_functions_set: HashSet<String>,
    native_function_table: HashMap<String, NativeFunction>,
    native_function_index_table: HashMap<String, usize>,
    type_table: HashMap<String, LinkedHashMap<String, TypeConstraint>>,
    type_index_table: HashMap<String, usize>,
    local_table: HashMap<String, usize>,
    global_table: HashMap<String, usize>,
    preserved_locals: HashSet<String>,
    current_function: Option<Function>,
    entry_point: String,
}

impl CodeGen {
    pub fn new(entry_point: String) -> Self {
        let mut function_table = HashMap::new();
        let mut function_index_table = HashMap::new();

        // this is to reserve the index 0 for the entrypoint function
        function_table.insert(
            entry_point.clone(),
            Function::new(entry_point.clone(), Vec::new()),
        );
        function_index_table.insert(entry_point.clone(), 0);

        let (native_function_table, native_function_index_table) = Self::get_native_function_maps();

        CodeGen {
            function_table: function_table,
            function_index_table: function_index_table,
            deferred_functions: Vec::new(),
            deferred_functions_set: HashSet::new(),
            native_function_table: native_function_table,
            native_function_index_table: native_function_index_table,
            type_table: HashMap::new(),
            type_index_table: HashMap::new(),
            local_table: HashMap::new(),
            global_table: HashMap::new(),
            preserved_locals: HashSet::new(),
            current_function: Some(Function::new(entry_point.clone(), Vec::new())),
            entry_point: entry_point,
        }
    }

    pub fn generate_from_node(&mut self, node: ParserNode) {
        self.accept_node(node);
        self.finalize_current();
    }

    pub fn dissolve(self) -> LeibnizRuntime {
        let function_table = self.function_table;
        let function_index_table = self.function_index_table;
        let mut finished_func_table = HashMap::new();

        for kvp in function_table {
            finished_func_table.insert(*function_index_table.get(&kvp.0).unwrap(), kvp.1);
        }

        let native_function_table = self.native_function_table;
        let native_function_index_table = self.native_function_index_table;
        let mut finished_native_func_table = HashMap::new();

        for kvp in native_function_table {
            finished_native_func_table
                .insert(*native_function_index_table.get(&kvp.0).unwrap(), kvp.1);
        }

        let type_table = self.type_table;
        let type_index_table = self.type_index_table;
        let mut finished_type_table = HashMap::new();

        for r#type in type_table {
            let fields = r#type.1.into_iter().map(|kvp| kvp.0).collect();

            finished_type_table.insert(*type_index_table.get(&r#type.0).unwrap(), fields);
        }

        LeibnizRuntime::new(
            finished_func_table,
            finished_native_func_table,
            finished_type_table,
        )
    }

    fn finalize_current(&mut self) {
        if self.current_function.is_none() {
            panic!("tried to finalize a function, but there wasn't one being built");
        }

        self.apply_basic_optimization();

        let func = self.current_function.take().unwrap();
        self.function_table.insert(func.name.clone(), func);

        self.local_table.clear();

        while self.deferred_functions.len() > 0 {
            let (name, parameters, node) = self.deferred_functions.pop().unwrap();

            self.current_function = Some(Function::new(name, parameters.clone()));

            for param in parameters {
                self.register_local(param);
            }

            self.generate_from_node(node);
        }
    }

    fn apply_basic_optimization(&mut self) {
        Optimization::apply_basic_optimization(self.current_function.as_mut().unwrap());
    }

    fn get_native_function_maps() -> (HashMap<String, NativeFunction>, HashMap<String, usize>) {
        let mut funcs = HashMap::new();

        funcs.insert(
            "print",
            NativeFunction::new(
                |parameters, runtime| {
                    println!("{}", parameters[0]);
                    runtime.push_value(parameters[0].clone());
                },
                1,
            ),
        );

        let funcs: HashMap<String, NativeFunction> = funcs
            .into_iter()
            .map(|kvp| (kvp.0.to_string(), kvp.1))
            .collect();

        let mut func_indexed = HashMap::new();
        let mut i = 0;

        for kvp in &funcs {
            func_indexed.insert(kvp.0.to_string(), i);
            i += 1;
        }

        (funcs, func_indexed)
    }

    fn accept_node(&mut self, node: ParserNode) {
        if self.current_function.is_none() {
            panic!("tried to generate code when no function has been created");
        }

        match node {
            ParserNode::Number(num, im) => self.accept_number(num, im),
            ParserNode::Identifier(identifier) => self.accept_identifier(&identifier),
            ParserNode::Operation(a, operator, b) => self.accept_operation(*a, *b, operator),
            ParserNode::Assignment(identifiers, value_node) => {
                self.accept_assignment(&identifiers, *value_node)
            }
            ParserNode::FunctionCall(identifier, parameter_nodes) => {
                self.accept_function_call(&identifier, parameter_nodes)
            }
            ParserNode::Conditional(predicate_node, true_node, false_node) => {
                self.accept_conditional(*predicate_node, *true_node, *false_node)
            }
            ParserNode::FunctionDeclaration(identifier, parameter_names, body) => {
                self.accept_function_declaration(identifier, parameter_names, *body)
            }
            ParserNode::TypeDeclaration(identifier, fields) => {
                self.accept_type_declaration(identifier, fields)
            }
            ParserNode::VariableDeclaration(identifier, expression_node) => {
                self.accept_variable_declaration(identifier, *expression_node)
            }
            ParserNode::Range(_, _, _, _) => {} // Ranges aren't actually emitted by the parser directly
            ParserNode::Array(expression_nodes) => self.accept_array(expression_nodes),
            ParserNode::Index(value_node, index_node) => {
                self.accept_array_index(*value_node, *index_node)
            }
            ParserNode::Loop(variable_name, range_node, body_node) => {
                self.accept_loop(variable_name, *range_node, *body_node)
            }
            ParserNode::Factorial(expression_node) => self.accept_factorial(*expression_node),
            ParserNode::Access(expression_node, field_name) => {
                self.accept_access(*expression_node, field_name)
            }
            ParserNode::Tree(tree_nodes) => self.accept_tree(tree_nodes),
        }
    }

    fn accept_number(&mut self, num: f64, im: bool) {
        self.emit_instr(Instruction::PushNumber(num));

        if im {
            self.emit_instr(Instruction::MakeImaginary);
        }
    }

    fn accept_identifier(&mut self, identifier: &str) {
        let has_local = self.has_local(identifier);
        let has_global = self.has_global(identifier);

        if !has_local && !has_global {
            panic!("tried to generate code to load variable '{}' when it hasn't been declared locally or globally", identifier);
        }

        if has_local {
            self.emit_instr(Instruction::GetLocal(self.local_index(identifier)));
        } else {
            self.emit_instr(Instruction::GetGlobal(self.global_index(identifier)));
        }
    }

    fn accept_operation(&mut self, a: ParserNode, b: ParserNode, operator: Operator) {
        self.accept_node(a);
        self.accept_node(b);

        let instr = match operator {
            Operator::Add => Instruction::Add,
            Operator::Subtract => Instruction::Subtract,
            Operator::Multiply => Instruction::Multiply,
            Operator::Divide => Instruction::Divide,
            Operator::Power => Instruction::Raise,
            Operator::Modulo => Instruction::Remainder,
            Operator::Equals => Instruction::Equals,
            Operator::GreaterThan => Instruction::GreaterThan,
            Operator::LessThan => Instruction::LessThan,
            Operator::GreaterThanOrEquals => Instruction::GreaterThanOrEquals,
            Operator::LessThanOrEquals => Instruction::LessThanOrEquals,
        };

        self.emit_instr(instr);
    }

    fn accept_assignment(&mut self, identifiers: &[String], value_node: ParserNode) {
        for identifier in identifiers {
            if !self.has_local(identifier) && !self.has_global(identifier) {
                panic!(
                    "tried to generate code to re-assign variable '{}' but it was not defined",
                    identifier
                );
            }
        }

        self.accept_node(value_node);

        for _ in 1..identifiers.len() {
            self.emit_instr(Instruction::Dupe);
        }

        for identifier in identifiers {
            if self.has_local(identifier) {
                self.emit_instr(Instruction::SetLocal(self.local_index(identifier)));
            } else {
                self.emit_instr(Instruction::SetGlobal(self.global_index(identifier)));
            }
        }
    }

    fn accept_function_call(&mut self, identifier: &str, parameter_nodes: Vec<ParserNode>) {
        let supplied_param_count = parameter_nodes.len();

        for node in parameter_nodes {
            self.accept_node(node);
        }

        if self.has_native_function(identifier) {
            let param_count = self
                .native_function_table
                .get(identifier)
                .unwrap()
                .parameters();

            if param_count != supplied_param_count {
                panic!(
                    "function '{}' expects {} parameters",
                    identifier, param_count
                );
            }

            self.emit_instr(Instruction::CallNative(
                self.native_function_index(identifier),
            ));
            return;
        } else if self.has_type(identifier) {
            self.emit_instr(Instruction::MakeType(self.type_index(identifier)));
            return;
        }

        if !self.has_function(identifier) && !self.deferred_functions_set.contains(identifier) {
            panic!(
                "tried to call function '{}' which hasn't been declared before",
                identifier
            );
        }

        self.emit_instr(Instruction::Call(self.function_index(identifier)));
    }

    fn accept_conditional(
        &mut self,
        predicate_node: ParserNode,
        true_node: ParserNode,
        false_node: ParserNode,
    ) {
        self.accept_node(predicate_node);

        let end_of_initial = self.cursor_pos();

        self.accept_node(true_node);
        let end_of_true = self.cursor_pos();

        self.accept_node(false_node);
        let end_of_false = self.cursor_pos();

        self.move_cursor_to(end_of_initial);
        self.emit_instr(Instruction::JumpFalse(
            (end_of_true - end_of_initial + 2) as isize,
        ));

        self.move_cursor_to(end_of_true + 1);
        self.emit_instr(Instruction::Jump((end_of_false - end_of_true + 1) as isize));

        self.move_cursor_to_end();
    }

    fn accept_function_declaration(
        &mut self,
        identifier: String,
        parameter_names: Vec<String>,
        body: ParserNode,
    ) {
        if self.has_function(&identifier) || self.has_native_function(&identifier) {
            panic!(
                "a function with the name '{}' has already been defined",
                identifier
            );
        }

        self.register_function(identifier.clone());
        self.deferred_functions
            .push((identifier.clone(), parameter_names, body));
        self.deferred_functions_set.insert(identifier);
    }

    fn accept_type_declaration(
        &mut self,
        identifier: String,
        fields: Vec<(String, TypeConstraint)>,
    ) {
        if self.has_type(&identifier) {
            panic!(
                "a type with the name '{}' has already been defined",
                identifier
            );
        }

        self.register_type(identifier, fields);
    }

    fn accept_variable_declaration(&mut self, identifier: String, expression_node: ParserNode) {
        let global_context = self.in_entry_point();

        if self.has_local(&identifier) {
            panic!(
                "cannot declare a variable '{}' as it has already been declared before locally",
                identifier
            );
        } else if self.has_global(&identifier) && global_context {
            panic!(
                "cannot declare a variable '{}' as it has already been declared before globally",
                identifier
            );
        }

        self.accept_node(expression_node);

        if global_context {
            let index = self.register_global(identifier);
            self.emit_instr(Instruction::SetGlobal(index));
        } else {
            let index = self.register_local(identifier);
            self.emit_instr(Instruction::SetLocal(index));
        }
    }

    fn accept_array(&mut self, expression_nodes: Vec<ParserNode>) {
        let len = expression_nodes.len();

        for expression in expression_nodes {
            self.accept_node(expression);
        }

        self.emit_instr(Instruction::MakeArray(len));
    }

    fn accept_array_index(&mut self, value_node: ParserNode, index_node: ParserNode) {
        self.accept_node(value_node);
        self.accept_node(index_node);
        self.emit_instr(Instruction::Index);
    }

    fn accept_loop(
        &mut self,
        variable_name: String,
        range_node: ParserNode,
        body_node: ParserNode,
    ) {
        if self.has_preserved_local(&variable_name) {
            panic!(
                "the local '{}' was already defined in an enclosing loop",
                &variable_name
            );
        }

        if self.has_local(&variable_name) {
            let index = self.local_index(&variable_name);
            self.emit_instr(Instruction::PreserveLocal(index));
            self.track_preserved_local(variable_name.clone());
        } else {
            self.register_local(variable_name.clone());
        }

        let index = self.local_index(&variable_name);
        let (lower_bound_node, upper_bound_node, step_node, going_down) =
            range_node.destructure_range();

        self.emit_instr(Instruction::PushNumber(0.0));

        self.accept_node(lower_bound_node);
        self.emit_instr(Instruction::SetLocal(index));

        let end_of_init = self.cursor_pos() as isize;

        self.accept_node(body_node);
        self.emit_instr(Instruction::Add);

        self.emit_instr(Instruction::GetLocal(index));
        self.accept_node(step_node);

        if going_down {
            self.emit_instr(Instruction::Subtract);
        } else {
            self.emit_instr(Instruction::Add);
        }

        self.emit_instr(Instruction::SetLocal(index));

        let end_of_body = self.cursor_pos() as isize;

        self.emit_instr(Instruction::GetLocal(index));
        self.accept_node(upper_bound_node);

        if going_down {
            self.emit_instr(Instruction::GreaterThanOrEquals);
        } else {
            self.emit_instr(Instruction::LessThanOrEquals);
        }

        let end_of_check = self.cursor_pos() as isize;
        self.emit_instr(Instruction::JumpTrue(-(end_of_check - end_of_init)));

        self.move_cursor_to(end_of_init as usize);
        self.emit_instr(Instruction::Jump(end_of_body - end_of_init + 1));

        self.move_cursor_to_end();

        if self.has_preserved_local(&variable_name) {
            self.emit_instr(Instruction::FetchLocal(index));
            self.emit_instr(Instruction::SetLocal(index));
            self.untrack_preserved_local(&variable_name);
        } else {
            self.remove_local(&variable_name);
        }
    }

    fn accept_factorial(&mut self, expression_node: ParserNode) {
        self.accept_node(expression_node);
        self.emit_instr(Instruction::PushNumber(1.0));
        self.emit_instr(Instruction::Add);
        self.emit_instr(Instruction::Gamma)
    }

    fn accept_access(&mut self, expression_node: ParserNode, field_name: String) {
        self.accept_node(expression_node);
        self.emit_instr(Instruction::LoadField(field_name));
    }

    fn accept_tree(&mut self, tree_nodes: Vec<ParserNode>) {
        for node in tree_nodes {
            self.accept_node(node);
        }
    }

    fn has_local(&self, identifier: &str) -> bool {
        self.local_table.contains_key(identifier)
    }

    fn local_index(&self, identifier: &str) -> usize {
        if !self.has_local(identifier) {
            panic!("no local by the name '{}' exists", identifier);
        }

        *self.local_table.get(identifier).unwrap()
    }

    fn register_local(&mut self, identifier: String) -> usize {
        if self.has_local(&identifier) {
            panic!(
                "tried to register local '{}' when it already exists",
                identifier
            );
        }

        let local_index = self.local_table.len();
        self.local_table.insert(identifier, local_index);

        local_index
    }

    fn remove_local(&mut self, identifier: &str) {
        if !self.has_local(identifier) {
            panic!("tried to remove local '{}' when it didn't exist");
        }

        self.local_table.remove(identifier);
    }

    fn track_preserved_local(&mut self, identifier: String) {
        if self.has_preserved_local(&identifier) {
            panic!(
                "tried to track preserved local '{}' when it was preserved before",
                identifier
            );
        }

        self.preserved_locals.insert(identifier);
    }

    fn untrack_preserved_local(&mut self, identifier: &str) {
        if !self.has_preserved_local(identifier) {
            panic!(
                "tried to untrack preserved local '{}' when it wasn't there",
                identifier
            );
        }

        self.preserved_locals.remove(identifier);
    }

    fn has_preserved_local(&self, identifier: &str) -> bool {
        self.preserved_locals.contains(identifier)
    }

    fn has_global(&self, identifier: &str) -> bool {
        self.global_table.contains_key(identifier)
    }

    fn global_index(&self, identifier: &str) -> usize {
        if !self.has_global(identifier) {
            panic!("no global by the name '{}' exists", identifier);
        }

        *self.global_table.get(identifier).unwrap()
    }

    fn register_global(&mut self, identifier: String) -> usize {
        if self.has_global(&identifier) {
            panic!(
                "tried to register global '{}' when it already exists",
                identifier
            );
        }

        let global_index = self.global_table.len();
        self.global_table.insert(identifier, global_index);

        global_index
    }

    fn has_function(&self, identifier: &str) -> bool {
        self.function_index_table.contains_key(identifier)
    }

    fn function_index(&self, identifier: &str) -> usize {
        if !self.has_function(identifier) {
            panic!("no function by the name '{}' exists", identifier);
        }

        *self.function_index_table.get(identifier).unwrap()
    }

    fn register_function(&mut self, identifier: String) -> usize {
        if self.has_function(&identifier) {
            panic!(
                "tried to register function '{}' when it already exists",
                identifier
            );
        }

        let function_index = self.function_index_table.len();
        self.function_index_table.insert(identifier, function_index);

        function_index
    }

    fn has_native_function(&self, name: &str) -> bool {
        self.native_function_index_table.contains_key(name)
    }

    fn native_function_index(&self, name: &str) -> usize {
        if !self.has_native_function(name) {
            panic!("no native function by the name '{}' exists", name);
        }

        *self.native_function_index_table.get(name).unwrap()
    }

    fn has_type(&self, identifier: &str) -> bool {
        self.type_table.contains_key(identifier)
    }

    fn type_index(&self, identifier: &str) -> usize {
        if !self.has_type(identifier) {
            panic!("no type by the name '{}' exists", identifier);
        }

        *self.type_index_table.get(identifier).unwrap()
    }

    fn register_type(
        &mut self,
        identifier: String,
        fields: Vec<(String, TypeConstraint)>,
    ) -> usize {
        if self.has_type(&identifier) {
            panic!(
                "tried to register type '{}' when it already exists",
                identifier
            );
        }

        let mut indexed_fields = LinkedHashMap::new();
        let mut i = 0;
        for field in &fields {
            indexed_fields.insert(field.0.clone(), i);
            i += 1;
        }

        let mut fields_as_map = LinkedHashMap::new();
        for field in fields {
            fields_as_map.insert(field.0, field.1);
        }

        let type_index = self.type_table.len();
        self.type_table.insert(identifier.clone(), fields_as_map);
        self.type_index_table.insert(identifier, type_index);

        type_index
    }

    fn move_cursor_to_end(&mut self) {
        self.current_function.as_mut().unwrap().move_cursor_to_end();
    }

    fn move_cursor_to(&mut self, index: usize) {
        self.current_function
            .as_mut()
            .unwrap()
            .move_cursor_to(index);
    }

    fn cursor_pos(&self) -> usize {
        self.current_function.as_ref().unwrap().cursor_pos()
    }

    fn emit_instr(&mut self, instruction: Instruction) {
        self.current_function
            .as_mut()
            .unwrap()
            .emit_instr(instruction);
    }

    fn in_entry_point(&self) -> bool {
        self.current_function.as_ref().unwrap().name == self.entry_point
    }
}
