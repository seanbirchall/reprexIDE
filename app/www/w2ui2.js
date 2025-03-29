HTMLWidgets.widget({
  name: 'tabEditor',
  type: 'output',

  factory: function(el, width, height) {
    // Initialize variables
    let ind = 2;

    // Create the tabs container
    const tabsContainer = document.createElement('div');
    tabsContainer.id = 'editor-tabs';
    el.appendChild(tabsContainer);

    // Initialize the tabs
    let tabs = new w2tabs({
      box: '#editor-tabs',
      name: 'tabs',
      active: 'tab1',
      reorder: false,
      tabs: [
        { id: 'tab1', text: 'script.R', closable: true },
        { id: 'add', text: '✚' },
      ]
    });

    function makeTabEditable(tabId) {
      let tab = w2ui['tabs'].get(tabId);
      if (!tab) {
        console.log(`Tab ${tabId} not found`);
        return;
      }

      function hasValidExtension(name) {
        return /\.(R|rmd|qmd|md|app|api|db|sql|js)$/i.test(name);
      }

      let newText = prompt("Edit file name: (must end with valid extension)", tab.text);
      while (newText !== null && !hasValidExtension(newText)) {
        newText = prompt("Valid extension: .R, .RMD, .QMD, .MD, .APP, .API, .DB, .JS or .SQL", tab.text);
      }

      if (newText !== null) {
        w2ui['tabs'].get(tabId).text = newText;
        w2ui['tabs'].refresh();
        if (HTMLWidgets.shinyMode) {
          Shiny.setInputValue(el.id + '_tab_edit', { tabId: tabId, newName: newText });
        }
      }
    }

    // Event handlers
    tabs.onClick = function(event) {
      event.done(() => {
        if (event.target === 'add') {
          let id = 'tab' + ind;
          this.insert('add', { id: id, text: 'script' + ind + '.R', closable: true });
          this.click(id);
          if (HTMLWidgets.shinyMode) {
            Shiny.setInputValue(el.id + '_tab', id);
          }
          ind++;
        } else {
          if (HTMLWidgets.shinyMode) {
            Shiny.setInputValue(el.id + '_tab', event.target);
          }
        }
      });
    };

    tabs.onClose = function(event) {
      let tab = w2ui['tabs'].get(event.target);
      if (confirm(`Are you sure you want to close: ${tab.text}?`)) {
        event.done(() => {
          w2ui['tabs'].remove(event.target);
          if (HTMLWidgets.shinyMode) {
            Shiny.setInputValue(el.id + '_tab_close', event.target);
          }
        });
      } else {
        event.preventDefault();
      }
    };

    tabs.onDblClick = function(event) {
      if (event.target !== 'add') {
        makeTabEditable(event.target);
      }
    };

    // Return widget instance for HTMLWidgets
    return {
      renderValue: function(x) {
        // Initialize tabs with provided values
        const tab_start = x.tabs;
        ind = Array.isArray(tab_start) ? tab_start.length + 1 : 2;

        if (tabs.get('tab1')) {
          tabs.remove('tab1');
        }
        if (tabs.get('add')) {
          tabs.remove('add');
        }

        if (Array.isArray(tab_start) && tab_start.length > 1) {
          tabs.add(
            tab_start.map((text, index) => ({
              id: 'tab' + (index + 1),
              text: text,
              closable: true
            }))
          );
        } else {
          tabs.add({
            id: 'tab1',
            text: Array.isArray(tab_start) ? tab_start[0] : tab_start,
            closable: true
          });
        }

        tabs.add({ id: 'add', text: '✚' });
        tabs.refresh();
        tabs.click('tab1');

        if (HTMLWidgets.shinyMode) {
          Shiny.setInputValue(el.id + '_ready', Date.now());
        }
      },

      resize: function(width, height) {
        // Handle resize if needed
      }
    };
  }
});
