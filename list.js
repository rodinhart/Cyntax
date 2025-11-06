// (deftype List [car cdr])
export function List(car, cdr) {
  if (!(this instanceof List)) return new List(car, cdr)

  this.car = car
  this.cdr = cdr
}
List["Type/invoke"] = ($, method, obj, args) =>
  method({ ...$, car: obj.car, cdr: obj.cdr, ...args })

List.prototype[Symbol.iterator] = function* () {
  let c = this
  while (c) {
    yield c.car
    c = c.cdr
  }
}

export const cons = (car, cdr) => List(car, cdr)
export const list = (...xs) =>
  [...xs].reverse().reduce((r, x) => List(x, r), null)
