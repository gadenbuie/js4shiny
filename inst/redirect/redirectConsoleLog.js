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

function escapeHTML (string) {
  return ('' + string)
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;');
}

const redirectLogger = (function(origConsole) {
  const consoleEvent = new Event('consoleLog');
  return function (logDiv, returnConsole = false) {
    if (!logDiv) return origConsole;
    let console = {
      log: function () {
        // https://stackoverflow.com/a/45387558/2022615
        let output = "";

        for (let arg of arguments) {
          if (arg instanceof Error) {
            output += `<span class="jslog-error">`;
          } else {
            output += `<span class="jslog-${typeof arg}">`;
          }

          if (arg instanceof Node) {
            let arg_obj = domToObj(arg);
            output = output.replace('(object)', '(Node)');
            output += JSON.stringify(arg_obj, null, 2);
          } else if (
            arg instanceof Error
          ) {
            output += arg.message;
          } else if (
            typeof arg === "object" &&
            typeof JSON === "object" &&
            typeof JSON.stringify === "function"
          ) {
            outJsonString = JSON.stringify(arg);
            if (outJsonString.length < 60) {
              output += escapeHTML(outJsonString);
            } else {
              output += escapeHTML(JSON.stringify(arg, null, 2));
            }
          } else {
            output += escapeHTML(arg);
          }

          output += "</span>&nbsp;";
        }

        logDiv.innerHTML += output + "<br>";
        logDiv.dispatchEvent(consoleEvent);
        //origConsole.log.apply(undefined, arguments);
      },
      clear: function() {
        logDiv.innerHTML = "";
      }
    };

    if (returnConsole) {
      return(console)
    };

    return function(code) {
      try {
        let ret = eval(code);
        if (typeof ret !== 'undefined') console.log(ret);
      }
      catch (error) {
        console.log(error);
      }
    };
  };
})(window.console);
