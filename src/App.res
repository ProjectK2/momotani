open Belt

let elems = [
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
]

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

let toFirstUpper = (s: string) => {
  s->String.charAt(0)->String.toUpperCase ++ s->String.sliceToEnd(~start=1)
}
let rec toTree = (s: string) => {
  switch String.length(s) {
  | 0 => None
  | 1 => Some(Leaf(s->String.toUpperCase))
  | 2 => {
      let l0 = Node(
        String.charAt(s, 0)->String.toUpperCase,
        [Leaf(String.charAt(s, 1)->String.toUpperCase)],
      )
      let l1 = Leaf(s->toFirstUpper)
      Some(Root([l0, l1]))
    }
  | _ => {
      let s0 = String.charAt(s, 0)->String.toUpperCase
      let rest0 = String.sliceToEnd(s, ~start=1)->toTree
      let t0 = Node(s0, unpackRoot(rest0)->Option.getExn)
      let s1 = s0 ++ String.slice(s, ~start=1, ~end=2)
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

type score = {
  isOk: bool,
  score: int,
}

let calcScore = (a: array<string>) => {
  let isOk = Js.Array2.every(a, s => Js.Array2.includes(elems, s))
  let score = 0
  {isOk, score}
}

@react.component
let make = () => {
  let (t, setT) = React.useState(() => "")
  let (tree, setTree) = React.useState(() => None)
  let (arr, setArr) = React.useState(() => [])
  let (okCount, setOkCount) = React.useState(() => 0)

  let makeElem = (s: string) => {
    if Js.Array2.includes(elems, s) {
      <span className="button is-success is-light"> {s->React.string} </span>
    } else {
      <span className="button is-danger is-light"> {s->React.string} </span>
    }
  }

  <div className="block">
    <h1 className="title"> {React.string("Modern MoMoTaNi")} </h1>
    <input
      type_="text"
      className={"input is-rounded is-large " ++ if arr->Js.Array2.length == 0 {
        ""
      } else if okCount == 0 && arr->Js.Array2.length > 0 {
        "is-danger"
      } else {
        "is-success"
      }}
      placeholder="e.g. momotani"
      onChange={event => {
        let v = ReactEvent.Form.target(event)["value"]->String.toLowerCase
        let tree = v->toTree
        let arr =
          tree
          ->Option.map(flatten)
          ->Option.getWithDefault([])
          ->Js.Array2.map(x => (calcScore(x), x))
          ->Js.Array2.sortInPlaceWith(((sa, a), (sb, b)) => {
            if sa.isOk === sb.isOk {
              sb.score - sb.score
            } else if sa.isOk {
              -1
            } else {
              1
            }
          })
        setT(_ => v)
        setTree(_ => tree)
        setArr(_ => arr)
        setOkCount(_ => arr->Js.Array2.filter(((s, _)) => {s.isOk})->Js.Array2.length)
      }}
    />
    <div className="box">
      <p> {(okCount->Int.toString ++ "/" ++ arr->Array.length->Int.toString)->React.string} </p>
      <ul className="content">
        {arr
        ->Array.map(((s, x)) => {
          <li className=""> {x->Array.map(makeElem)->React.array} </li>
        })
        ->React.array}
      </ul>
    </div>
  </div>
}
