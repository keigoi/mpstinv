[@warning "-27"];

let rec ta = () =>
  `B({
    pub left = v => {
      Printf.printf("%d\n", v);
      `B(`ack(((), ta())));
    };
    pub right = v => {
      print_endline(v);
      ();
    }
  });

let rec tb = cnt =>
  if (cnt == 0) {
    `A(`right(("finish", ())));
  } else {
    `A(`left((cnt, `A({pub ack = () => tb(cnt - 1)}))));
  };

open Domainslib;
let () = {
  let a2b = Chan.make_unbounded();
  let b2a = Chan.make_unbounded();
  let rec a_handler = h => {
    let `B(h) = h;
    switch (Chan.recv(b2a)) {
    | `left(v) =>
      let `B(`ack(v, h)) = h#left(v);
      Chan.send(a2b, `ack(v));
      a_handler(h);
    | `right(v) => h#right(v)
    };
  };

  let rec b_handler = h => {
    let `A(h) = h;
    switch (h) {
    | `left(v, h) =>
      Chan.send(b2a, `left(v));
      let `ack(v) = Chan.recv(a2b);
      let `A(h) = h;
      b_handler(h#ack(v));
    | `right(v, ()) => Chan.send(b2a, `right(v))
    };
  };

  let t = Thread.create(a_handler, ta());
  b_handler(tb(10));
  Thread.join(t);
};

type local('t) = {run: 'k. ('t => 'k) => 'k};
