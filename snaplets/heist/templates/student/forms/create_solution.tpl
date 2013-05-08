<apply template="/student/base">
  <h3>Lösen von <task-name /></h3>

  <dfForm id="task-config-form">
    <div class="well control-group form-horizontal">
      <label class="control-label" for="form.name">
        Aufgabenstellung
      </label>
      <div class="controls docs">
        <p><taskDescription /></p>
      </div>
    </div>

    <div class="well control-group form-horizontal">
      <label class="control-label" for="form.name">
        Dokumentation
      </label>
      <div class="controls docs">
        <p><taskDocumentation /></p>
      </div>
    </div>

    <div class="well control-group form-horizontal">
      <label class="control-label" for="solution">
        Lösung
      </label>
      <div class="controls">
        <dfInputTextArea id="task-config" ref="solution" />
      </div>
    </div>

    <verificationError />

    <evaluation />

    <div class="button-row">
      <dfInputSubmit value="Beispiellösung laden"
                     class="btn"
                     name="btn_reset" />
      <dfInputSubmit value="Lösung einsenden"
                     class="btn btn-primary" />
    </div>
  </dfForm>
</apply>
