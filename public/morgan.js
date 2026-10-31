let m = document;
m.qsa = m.querySelectorAll;
m.bid = m.getElementById;

function ok_welcome(e) {
    console.log("ok welcome");
}

function close_welcome(e) {
    console.log("goodbye :c")
    m.bid("welcome").remove();
}


document.addEventListener("DOMContentLoaded", () => {
    console.log("DOMContentLoaded");
    m.bid("welcome-ok").addEventListener("click", ok_welcome);
    m.bid("close-ok").addEventListener("click", close_welcome);
    console.log("added event listeners");
});
