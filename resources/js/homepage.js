'use strict'

function del(type, name, self) {

    $(self).parents('tr').remove()

    var content = '[{"op": "remove","path": "/' + type + '/' + name + '"}]'

    $.ajax('/api/userInfo', {
        type: 'POST',
        contentType: 'application/json-patch+json',
        data: content
    })
}

$(document).ready(function() {

    var env = {
        vars: {},
        funcs: {}
    }

    var buildTable = function() {
        var tableContent = ""

        function removeButton(type, name) {
            var button = '<a onclick="'
            button += 'del(&quot;' + type + '&quot;, &quot;' + name + '&quot;, this)'
            button += '"><span class="glyphicon glyphicon-remove"></span></a>'
            return button;
        }

        $('#vars').empty()
        for (var key in env.vars) {
            tableContent += '<tr><td>' + key + '</td><td>' + env.vars[key] + removeButton('var', key) + '</td></tr>'
        }
        $('#vars').html(tableContent)

        tableContent = ""
        $('#funcs').empty()
        for (var key in env.funcs) {
            var eqn = (key + env.funcs[key]).split(' = ')
            tableContent += '<tr><td>' + eqn[0] + '</td><td>' + eqn[1] + removeButton('func', key) + '</td></tr>'
        }
        $('#funcs').html(tableContent)
    }

    var addToEnv = function(vals) {
        for (var key in vals.newVars) {
            env.vars[key] = vals.newVars[key]
        }

        for (var key in vals.newFuncs) {
            env.funcs[key] = vals.newFuncs[key]
        }

        buildTable()
    }

    $('#math-form').submit(function (event) {

        // Stop form from submitting normally
        event.preventDefault()

        var url = $(this).attr('action')

        var posting = $.post(url, $(this).serialize())
        posting.done(function (data) {

            addToEnv(data)

            var $result = $('#result').empty()

            $result.removeClass('error')
                    .append(data.result)
        });

        posting.fail(function (res) {
            var $result = $('#result').empty()

            $result.addClass('error')
                   .append(res.responseJSON.error)
        });

    })

    $('#reset-vars').click(function () {
        $.ajax('/api/userInfo', { method: 'DELETE' })

        $('#vars').empty()
        $('#funcs').empty()
    })

    $.get('/api/userInfo', function(data) {
        addToEnv(data)
    })
})
