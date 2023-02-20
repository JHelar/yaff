# Yes Another Front-end Framework

Yaff is my deep dive into the Rust programming language.
Also techings in how to create your own parser, lexer and compiler.

## What does it do?
Yaff is a templating language to create html output using components. So far it does not support data fetching, bindings or any other type of language logic, it just a simple templating language that extends regular html.

## Usage
### Prerequisite
You need the `rustc` compiler in order to compile the program

### Compile
```bash
rustc yaff.rs
```

## Run
```bash
./yaff input.yaff
```

## Syntax

### Component
You can use components to simply reuse html code, it supports props by name. A prop can be anything including other components.


```html
comp greeting(name) = <h1>Hello {name}</h1>
<div>
    <greeting name="Foo"/>
    <greeting name={<b>Bar</b>}/>
</div>
```
outputs
```html
<div>
    <h1>Hello Foo</h1>
    <h1>Hello <b>Bar</b></h1>
</div>
```

You can also nestle components within each other using the `{comp:children}` keyword.
```html
comp subheading = <h2>Subheading</h2>
comp greeting(name) = <div>
    <h1>Hello {name}</h1>
    {comp:children}
</div>

<div>
    <greeting name="Foo"><subheading /></greeting>
</div>
```
outputs
```html
<div>
    <div>
        <h1>Hello Foo</h1>
        <h2>Subheading</h2>
    </div>
</div>
```