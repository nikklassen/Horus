'use strict'

$(document).ready(function() {

    var env = {
        vars: {},
        funcs: {}
    }

    var buildTable = function() {
        var tableContent = ""
        $('#vars').empty()
        for (var key in env.vars) {
            tableContent += '<tr><td>' + key + '</td><td>' + env.vars[key] + '</td></tr>'
        }
        $('#vars').html(tableContent)

        tableContent = ""
        $('#funcs').empty()
        for (var key in env.funcs) {
            tableContent += '<tr><td>' + key + '</td><td>' + env.funcs[key] + '</td></tr>'
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
            var vals = JSON.parse(data)
            addToEnv(vals)

            var $result = $('#result').empty()

            var content = ""
            if (vals.error !== undefined) {
                $result.addClass('error')
                       .append(vals.error)
            } else {
                $result.removeClass('error')
                       .append(vals.result)
            }
        });
    })

    $.get('/api/userInfo', function(data) {
        var vals = JSON.parse(data)
        addToEnv(vals)
    })
})
