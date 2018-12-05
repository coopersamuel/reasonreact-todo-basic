type todo = {
  id: int,
  text: string,
  completed: bool,
};

type state = {todos: list(todo)};

type action =
  | Add(string)
  | Check(int)
  | Delete(int);

/* The ref function denotes a mutable value, ie. todoId is a *mutable* int type */
let todoId = ref(0);

let newTodo = text => {
  /* := allows us to reassign the todoId reference */
  /* ^ tells the compiler that this is a reference */
  todoId := todoId^ + 1;
  {id: todoId^, completed: false, text}; /* { text } is shorthand for { text: text } */
};

let check = (id, todos) =>
  List.map(todos, t => t.id == id ? {...t, completed: !t.completed} : t);

let delete = (id, todos) => List.keep(todos, t => t.id != id);

/*
   e : string assigns a type of string to e

   -> is the fast pipe operator, basically a FP way to pass args to a function
   e -> fun is equivalent to fun(e)
   Similarly, e -> fun1 -> fun2 == fun2(fun1(e))

   ## is an accessor for JS objects
 */
let valueFromEvent = e: string => ReactEvent.Form.target(e)##value;

module Input = {
  type state = { value: string };
  type action = 
    | Change(string)
    | Clear;

  let component = ReasonReact.reducerComponent("Input");

  let make = (~onSubmit, _children) => {
    ...component,
    initialState: () => { value: "" },
    reducer: (action, state) => 
      switch action {
      | Change(text) => ReasonReact.Update({ value: text })
      | Clear => ReasonReact.Update({ value: "" })
      },
    render: self =>
      <input
        className="input"
        value={self.state.value}
        type_="text"
        placeholder="What do you want todo"
        onChange=(e => self.send(Change(ReactEvent.Form.target(e)##value)))
        onKeyDown={
          e =>
            if (ReactEvent.Keyboard.key(e) == "Enter") {
              onSubmit(self.state.value);
              self.send(Clear);
            }
        }
      />,
  };
};

module TodoItem = {
  let component = ReasonReact.statelessComponent("TodoItem");

  let make = (~todo: todo, ~onToggle, ~clickDelete, _children) => {
    ...component,
    render: _self => {
      <div className="item" onClick={_e => onToggle(todo)}>
        <input
          className="checkbox"
          type_="checkbox"
          checked=(todo.completed)
        />
        <label> (ReasonReact.string(todo.text)) </label>
        <input
          type_="button"
          className="btn-delete"
          value="x"
          onClick={_e => clickDelete(todo)}
        />
      </div>
    }
  };
};

let component = ReasonReact.reducerComponent("App");

let make = _children => {
  ...component,
  initialState: () => {todos: []},
  reducer: (action, {todos}) =>
    switch (action) {
    | Add(text) => ReasonReact.Update({todos: [newTodo(text), ...todos]})
    | Check(id) => ReasonReact.Update({todos: check(id, todos)})
    | Delete(id) => ReasonReact.Update({todos: delete(id, todos)})
    },
  render: self =>
    <div className="App"> 
      <h3> {ReasonReact.string("Todo App")} </h3>
      <Input onSubmit=(todo => self.send(Add(todo))) />
      <div className="todoList">
        {
          List.map(
            self.state.todos,
            todo => 
              <TodoItem
                key=(string_of_int(todo.id))
                todo
                onToggle={todo => self.send(Check(todo.id))}
                clickDelete={todo => self.send(Delete(todo.id))}
              />
          )
          -> Belt.List.toArray
          -> ReasonReact.array
        }
      </div>
    </div>,
};