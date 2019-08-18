const nodeListInclude = ['id', 'tagName', 'className', 'childNodes'];

function domToObj (del) {
  // https://stackoverflow.com/a/46881092/2022615
  let obj = {};
  for (let prop of nodeListInclude) {
    if (del[prop] instanceof NodeList) {
      obj[prop] = Array.from(del[prop]);
    } else {
      obj[prop] = del[prop];
    }
  }
  return obj;
}

const redirectLogger = (function(origConsole) {
  return function (logDiv) {
    let console = {
      log: function () {
        // https://stackoverflow.com/a/45387558/2022615
        let output = "";

        for (let arg of arguments) {
          output += `<span class="jslog-${typeof arg}">(${typeof arg}) `;

          if (arg instanceof Node) {
            let arg_obj = domToObj(arg);
            output = output.replace('(object)', '(Node)');
            output += JSON.stringify(arg_obj);
          } else if (
            typeof arg === "object" &&
            typeof JSON === "object" &&
            typeof JSON.stringify === "function"
          ) {
            output += JSON.stringify(arg);
          } else {
            output += arg;
          }

          output += "</span>&nbsp;";
        }

        logDiv.innerHTML += output + "<br>";
        //origConsole.log.apply(undefined, arguments);
      }
    }

    return function(code) {
      eval(code)
    }
  }
})(window.console)
