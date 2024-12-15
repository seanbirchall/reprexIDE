document.addEventListener("keydown", function(event) {
  if(event.ctrlKey && event.key === "i"){
    event.preventDefault();
    document.getElementById("editor-import").click();
  }
  if(event.ctrlKey && event.shiftKey && event.key === "Enter"){
    event.preventDefault();
    document.getElementById("editor-run").click();
  }
  if (event.ctrlKey && event.key === "Enter") {
    event.preventDefault();
    const applyButton = document.getElementById("modal-modal_df_viewer-apply");
    if (applyButton) {
      applyButton.click();
    }
  }
  if (event.ctrlKey && event.shiftKey && (event.key === "S" || event.key === "s")) {
    event.preventDefault();
    document.getElementById("style").click();
  }
  //if(event.ctrlKey && event.key === "s"){
  //  event.preventDefault();
  //  document.getElementById("share").click();
  //}
});

document.addEventListener('paste', function(event) {
    setTimeout(() => {
        document.getElementById("style").click();
    }, 0);
});


