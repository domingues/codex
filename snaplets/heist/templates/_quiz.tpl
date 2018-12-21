
<apply template="_base">
  <mathjax-js/>
  <div class="description">
    <quiz-preamble/>
  </div>

  <p class="info">
    <current-timing>
      <Early>
	Submissões visíveis após <valid-from/>.
      </Early>
      <Valid>
	Submissões terminam em <valid-until/> (<time-left/>).
    </Valid>
      <Overdue>
	Submissões terminaram em <valid-until/>.
      </Overdue>
    </current-timing>
  </p>
  
  
  <form id="quiz" method="POST" action="${page-url}">
    <questions>
      <fieldset>
	<question-preamble/>
	<ol class="answers" type="${list-type}" start="${list-start}">
	  <alternatives>
	    <li>
	      <label>
		<if-checked>
		  <input type="checkbox" name="${question-name}" value="${alternative-label}" onclick="${onclick-callback}" checked/>
		  <else/>
		  <input type="checkbox" name="${question-name}" value="${alternative-label}" onclick="${onclick-callback}"/>
		</if-checked>&nbsp;<alternative/>
	      </label>
	    </li>
	  </alternatives>
	</ol>
      </fieldset>
    </questions>
    <div>
      <p><input type="submit" value="Submeter"/> &emsp;
	<a href="${page-parent-url}"
	   class="button">Voltar à página de índice</a> &emsp;
	<input type="button" value="Limpar seleção" style="float:right"
	       onclick="resetAll()"/>
      </p>
    </div>
  </form>

  <script>
    function onlyOne(checkbox) {
    var array = document.forms["quiz"].getElementsByTagName("input");
    for (i = 0; i<array.length; i++) {
	var item= array[i];
	if (item !== checkbox && item.name == checkbox.name) item.checked = false
    }
 }

   function resetAll() {
    var array = document.forms["quiz"].getElementsByTagName("input");
    for (i = 0; i<array.length; i++) {
	var item= array[i];
	if (item.type == "checkbox") item.checked = false
    }
   }				
  </script>
</apply>


<apply template="_browse">
  <li><a title="Editar a página de exercício" href="${file-url}">Editar</a></li>
</apply>
