import "core-js/stable";
import hljs from "highlight.js/lib/core";
import delphi from "highlight.js/lib/languages/delphi";
import { diffLines } from "diff";

function highlightTag(tagName) {
  const elements = document.getElementsByTagName(tagName);
  for(const element of elements) {
    hljs.highlightElement(element, { language: "delphi" });
  }
}

function getExampleElements() {
  let noncompliants = {};
  let compliants = {};

  const pres = document.querySelectorAll("pre[data-diff-id]");
  for(const pre of pres) {
    const id = pre.dataset.diffId;

    if(pre.dataset.diffType == "compliant") {
      compliants[id] = pre;
    } else if (pre.dataset.diffType == "noncompliant") {
      noncompliants[id] = pre;
    } else {
      console.log("Example found with no valid data-diff-type?");
      console.log(pre);
    }
  }

  let examplePairs = [];

  for(const key of Object.keys(noncompliants)) {
    if(compliants[key] == undefined) {
      continue;
    }

    examplePairs.push({
      noncompliant: noncompliants[key],
      compliant: compliants[key],
    });
  }

  return examplePairs;
}

function generateExampleDiffs() {
  const examplePairs = getExampleElements();

  for(const pair of examplePairs) {
    const hunks = diffLines(pair.noncompliant.innerHTML, pair.compliant.innerHTML);

    let noncompliantCode = "";
    let compliantCode = "";

    for(const hunk of hunks) {
      if(!hunk.added && !hunk.removed) {
        noncompliantCode += hunk.value;
        compliantCode += hunk.value;
      } else if(hunk.added) {
        compliantCode += `<div class="code-added">${hunk.value}</div>`;
      } else if(hunk.removed) {
        noncompliantCode += `<div class="code-removed">${hunk.value}</div>`;
      }
    }

    pair.compliant.innerHTML = compliantCode;
    pair.noncompliant.innerHTML = noncompliantCode;
  }
}

window.onload = function() {
  hljs.registerLanguage("delphi", delphi);
  highlightTag("pre");
  highlightTag("code");
  generateExampleDiffs();
}