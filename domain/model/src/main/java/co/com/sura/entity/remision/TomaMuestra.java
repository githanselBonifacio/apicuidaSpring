package co.com.sura.entity.remision;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class TomaMuestra {
    @JsonIgnore
    private Integer idTomaMuestra;
    private String idCita;
    private String tipoMuestra;
    private boolean requiereAyuno;
    private String tipoPrestacion;
}
