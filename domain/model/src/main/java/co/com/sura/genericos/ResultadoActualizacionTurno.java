package co.com.sura.genericos;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class ResultadoActualizacionTurno {
    private String idProfesional;
    private LocalDate fechaTurno;
    private String mensaje;
    public static Boolean isNotNull(ResultadoActualizacionTurno resultado){
        return resultado.mensaje!=null && resultado.fechaTurno !=null;
    }
}
