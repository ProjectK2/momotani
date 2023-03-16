open Belt

let elems =
  [
    "H",
    "He",
    "Li",
    "Be",
    "B",
    "C",
    "N",
    "O",
    "F",
    "Ne",
    "Na",
    "Mg",
    "Al",
    "Si",
    "P",
    "S",
    "Cl",
    "Ar",
    "K",
    "Ca",
    "Sc",
    "Ti",
    "V",
    "Cr",
    "Mn",
    "Fe",
    "Co",
    "Ni",
    "Cu",
    "Zn",
    "Ga",
    "Ge",
    "As",
    "Se",
    "Br",
    "Kr",
    "Rb",
    "Sr",
    "Y",
    "Zr",
    "Nb",
    "Mo",
    "Tc",
    "Ru",
    "Rh",
    "Pd",
    "Ag",
    "Cd",
    "In",
    "Sn",
    "Sb",
    "Te",
    "I",
    "Xe",
    "Cs",
    "Ba",
    "Hf",
    "Ta",
    "W",
    "Re",
    "Os",
    "Ir",
    "Pt",
    "Au",
    "Hg",
    "Tl",
    "Pb",
    "Bi",
    "Po",
    "At",
    "Rn",
    "Fr",
    "Ra",
    "Rf",
    "Db",
    "Sg",
    "Bh",
    "Hs",
    "Mt",
    "Ds",
    "Rg",
    "Cn",
    "Nh",
    "Fl",
    "Mc",
    "Lv",
    "Ts",
    "Og",
    "La",
    "Ce",
    "Pr",
    "Nd",
    "Pm",
    "Sm",
    "Eu",
    "Gd",
    "Tb",
    "Dy",
    "Ho",
    "Er",
    "Tm",
    "Yb",
    "Lu",
    "Ac",
    "Th",
    "Pa",
    "U",
    "Np",
    "Pu",
    "Am",
    "Cm",
    "Bk",
    "Cf",
    "Es",
    "Fm",
    "Md",
    "No",
    "Lr",
  ]->Array.map(s => String.toLowerCase(s))

type rec tree =
  | Root(array<tree>)
  | Leaf(string)
  | Node(string, array<tree>)

let unpackRoot = (t: option<tree>) => {
  switch t {
  | None => None
  | Some(Root(a)) => Some(a)
  | Some(t) => Some([t])
  }
}

let rec toTree = (s: string) => {
  switch String.length(s) {
  | 0 => None
  | 1 => Some(Leaf(s))
  | 2 => {
      let l0 = Node(String.charAt(s, 0), [Leaf(String.charAt(s, 1))])
      let l1 = Leaf(s)
      Some(Root([l0, l1]))
    }
  | _ => {
      let s0 = String.charAt(s, 0)
      let rest0 = String.sliceToEnd(s, ~start=1)->toTree
      let t0 = Node(s0, unpackRoot(rest0)->Option.getExn)
      let s1 = String.slice(s, ~start=0, ~end=2)
      let rest1 = String.sliceToEnd(s, ~start=2)->toTree
      let t1 = Node(s1, unpackRoot(rest1)->Option.getExn)
      Some(Root([t0, t1]))
    }
  }
}

let flatten = (t: tree) => {
  let rec f = (t: tree, acc: array<string>, result: array<array<string>>) => {
    switch t {
    | Root(x) =>
      Js.Array2.forEach(x, v => {
        // assert (acc === [])
        f(v, acc, result)
      })
    | Leaf(x) => {
        let acc' = Array.copy(acc)
        Array.push(acc', x)
        Array.push(result, acc')
      }
    | Node(s, a) => {
        Array.push(acc, s)
        Js.Array2.forEach(a, v => {
          f(v, acc, result)
        })
        assert (Js.Array2.pop(acc) === Some(s))
      }
    }
  }

  let a = []
  f(t, [], a)
  a
}

@react.component
let make = () => {
  let (t, setT) = React.useState(() => "")
  let (tree, setTree) = React.useState(() => None)
  let (arr, setArr) = React.useState(() => [])

  let toFirstUpper = (s: string) => {
    s->String.charAt(0)->String.toUpperCase ++ s->String.sliceToEnd(~start=1)
  }
  let makeElem = (s: string) => {
    if Js.Array2.includes(elems, s) {
      <span className="button is-success is-light"> {s->toFirstUpper->React.string} </span>
    } else {
      <span className="button is-danger is-light"> {s->toFirstUpper->React.string} </span>
    }
  }

  <div className="block">
    <h1 className="title"> {React.string("Modern MoMoTani")} </h1>
    <input
      type_="text"
      className="input is-rounded is-large"
      placeholder="e.g. momotani"
      onChange={event => {
        let v = ReactEvent.Form.target(event)["value"]->String.toLowerCase
        let tree = v->toTree
        let arr = tree->Option.map(flatten)
        setT(_ => v)
        setTree(_ => tree)
        setArr(_ => Option.getWithDefault(arr, []))
        Js.log(v)
        Js.log(tree)
        Js.log(arr)
      }}
    />
    <div className="box">
      <ul className="content">
        {arr
        ->Array.map(x => {
          <li className=""> {x->Array.map(makeElem)->React.array} </li>
        })
        ->React.array}
      </ul>
    </div>
  </div>
}
