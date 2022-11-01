// theme toggle
const themeKey = "danaswap-ui-theme";
const currentTheme = localStorage.getItem(themeKey);
const themeCss = document.getElementById("theme-css");
const themeToggleBtn = document.getElementById("theme-toggle-btn");

if (typeof currentTheme == "undefined" || currentTheme == "night") {
  themeToggleBtn.firstChild.innerText = "light mode";
  themeCss.href = "./assets/styles/night.css";
} else {
  themeToggleBtn.firstChild.innerText = "night mode";
  themeCss.href = "./assets/styles/light.css";
}

themeToggleBtn.addEventListener("click", function (e) {
  const currentTheme = localStorage.getItem(themeKey);

  if (typeof currentTheme == "undefined" || currentTheme == "night") {
    themeToggleBtn.firstChild.innerText = "night mode";
    themeCss.href = "./assets/styles/light.css";
    localStorage.setItem(themeKey, "light");
    themeToggleBtn.blur();
  } else {
    themeToggleBtn.firstChild.innerText = "light mode";
    themeCss.href = "./assets/styles/night.css";
    localStorage.setItem(themeKey, "night");
    themeToggleBtn.blur();
  }
});
