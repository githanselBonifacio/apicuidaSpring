package co.com.sura.agenda.entity;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class Tarea {
    private String    id;
    private LocalDateTime fechaInicio;
    private LocalDateTime fechaProgramada;
    private Integer   duracion;
    private Integer   holgura;
    private Double    latitud;
    private Double    longitud;
    private Integer   idEstado;
    private String    tipo;
}
