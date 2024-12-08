HTMLWidgets.widget({

  name: 'ctrShowOneTrialWidget',
  type: 'output',

  factory: function (el, width, height) {

    // helper function
    function jsonToJsTreeObject(json_data_object, path, final_array = new Array()) {

      Object.keys(json_data_object).forEach(function (data) {

        // Array.isArray(json_data_object[data]) || json_data_object[data] instanceof Object
        if (json_data_object[data] instanceof Object) {
          final_array.push({
            id: path + '.' + `${data}`,
            text: `${data}`,
            children: jsonToJsTreeObject(json_data_object[data], path + '.' + `${data}`, new Array())
          });
        }
        else {
          final_array.push({
            id: path + '.' + `${data}`,
            text: `${data}` + ': ' + `${json_data_object[data]}`
            // text: `${data}`,
            // children: [{ text: `${json_data_object[data]}` }]
          });
        }
      });

      return final_array;
    }

    // widget
    return {

      renderValue: function (x) {

        var selectedFields;

        el.innerHTML =
          '<input type="text" value="" id="jst_q" size="50" placeholder="Search field names or values"> ' +
          '<button>Copy selected fields to clipboard for use in ctrdata::dbGetFieldsIntoDf()</button> ' +
          '<div id="jst"></div>';

        $('#jst').jstree({
          "core": { "data": jsonToJsTreeObject({ trial: x.data }) },
          // https://www.jstree.com/plugins/
          "plugins": ["wholerow", "checkbox", "search"],
          // https://www.jstree.com/api/#/?q=$.jstree.defaults.checkbox
          "checkbox": { "three_state": false, "cascade": "undetermined" }
        });

        $('#jst').on("changed.jstree", function (e, data) {
          selectedFields = JSON.stringify(data.selected);
          selectedFields = selectedFields.replaceAll(/undefined[.]trial[.]?/g, '');
          selectedFields = selectedFields.replaceAll(/"",|,""|""/g, '');
          selectedFields = selectedFields.replaceAll(/[.][0-9]+/g, '');
          selectedFields = selectedFields.replace('[', '').replace(']', '');
          selectedFields = selectedFields.split(',');
          selectedFields = new Set(selectedFields);
          selectedFields = [...selectedFields];
          selectedFields = selectedFields.join(', ');
          selectedFields = 'fields = c(' + selectedFields + ')';
          console.log(selectedFields);

        });

        $('button').on('click', function () {
          // console.log(selectedFields);
          navigator.clipboard.writeText(selectedFields);
        });

        var to = false;
        $('#jst_q').keyup(function () {
          if (to) { clearTimeout(to); }
          to = setTimeout(function () {
            var v = $('#jst_q').val();
            $('#jst').jstree(true).search(v);
          }, 250);
        });

      },

      resize: function (width, height) { }

    };
  }
});
