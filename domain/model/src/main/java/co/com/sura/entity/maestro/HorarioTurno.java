package co.com.sura.entity.maestro;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.time.LocalTime;


@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class HorarioTurno {
    private Integer id;
    private String nombre;
    private LocalTime horaInicio;
    private LocalTime horaFin;
}
