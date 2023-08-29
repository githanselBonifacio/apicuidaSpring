package co.com.sura.entity.remision;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class Sondaje {
    @JsonIgnore
    private Integer idSondaje;
    private String idCita;
    private String tipoSondaje;
    private String tipoSonda;
    private Integer totalSesiones;
    private String tipoPrestacion;
}
