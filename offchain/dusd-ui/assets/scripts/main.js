const modeKey = "dusd-mode";
const currentMode = localStorage.getItem(modeKey);
const lightmodeCss = document.getElementById("lightmode-css");
const lightmodeToggleBtn = document.getElementById("lightmode-toggle-btn");

if (typeof currentMode == "undefined" || currentMode == "night") {
  lightmodeCss.href = "./assets/styles/night.css";
} else {
  lightmodeCss.href = "./assets/styles/light.css";
}

lightmodeToggleBtn.addEventListener("click", function (e) {
  const currentMode = localStorage.getItem(modeKey);

  if (typeof currentMode == "undefined" || currentMode == "night") {
    lightmodeCss.href = "./assets/styles/light.css";
    localStorage.setItem(modeKey, "light");
    lightmodeToggleBtn.blur();
  } else {
    lightmodeCss.href = "./assets/styles/night.css";
    localStorage.setItem(modeKey, "night");
    lightmodeToggleBtn.blur();
  }
});
