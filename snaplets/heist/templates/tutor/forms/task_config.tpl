<apply template="/tutor/base">
  <h3>Konfiguration einer Aufgabe vom Typ <task-name /></h3>

  <dfForm id="task-config-form">
    <div class="well control-group form-horizontal">
      <label class="control-label" for="form.name">
        Dokumentation
      </label>
      <div class="controls docs">
        <p><task-documentation /></p>
      </div>
    </div>

    <div class="well control-group form-horizontal">
      <label class="control-label" for="form.name">
        Titel
      </label>
      <div class="controls">
        <dfInputText ref="name" />
      </div>
    </div>

    <div class="well control-group form-horizontal">
      <label class="control-label" for="form.name">
        Konfiguration
      </label>
      <div class="controls">
        <dfInputTextArea id="task-config" ref="config" />
      </div>
    </div>

    <verification-error />

    <div class="button-row">
      <dfInputSubmit value="Konfiguration zuruecksetzen"
                     class="btn"
                     name="btn_reset" />
      <dfInputSubmit value="Aufgabe erstellen"
                     class="btn btn-primary" />
    </div>
  </dfForm>
</apply>
