package co.com.sura.entity.admin;

import com.fasterxml.jackson.annotation.JsonIgnore;
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
