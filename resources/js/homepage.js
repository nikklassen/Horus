'use strict'

$(document).ready(function() {

    var getCookieValues = function(cookieName) {
        var cookieVal = $.cookie(cookieName) || []

        var vals = JSON.parse(cookieVal)
        var variableTable = $('#vars').html()
        $.each(vals, function(n, v) {
            variableTable += '<tr><td>' + v[0] + '</td><td>' + v[1] + '</td></tr>'
        })

        $('#vars').html(variableTable)
    }

    var refresh = function() {
        $('#vars').empty()
        getCookieValues('vars')
        getCookieValues('funcs')
    }

    refresh()

    $('#math-form').submit(function (event) {

        // Stop form from submitting normally
        event.preventDefault()

        var url = $(this).attr('action')

        var posting = $.post(url, $(this).serialize())
        posting.done(function (data) {
            $('#result')
                .empty()
                .removeClass('error')
                .append(data)

            refresh()
        });
        posting.fail(function() {
            $('#result')
                .empty()
                .addClass('error')
                .append('Invalid input')
        })
    })

    $('#reset-vars').click(function () {
        $.removeCookie('vars')
        refresh()
    })
})
