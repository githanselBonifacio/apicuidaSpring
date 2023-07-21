package co.com.sura.postgres.repository.agenda.data;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Column;
import org.springframework.data.relational.core.mapping.Table;

import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
@Table("desplazamiento")
public class DesplazamientoData {
    @Id
    @Column("id_desplazamiento")
    private Integer idDesplazamiento;

    @Column("id_cita_partida")
    private String idCitaPartida;

    @Column("id_cita_destino")
    private String idCitaDestino;

    @Column("fecha_programada")
    private LocalDateTime fechaProgramada;

    @Column("id_horario_turno")
    private Integer idHorarioTurno;

    private String tipo;
    private Integer duracion;
    private Integer holgura;

    public static DesplazamientoData crearDesplazamientoData (CitaData citaPartida ,CitaData citaDestino){
        return DesplazamientoData
                .builder()
                .idCitaPartida(citaPartida.getIdCita())
                .idCitaDestino(citaDestino.getIdCita())
                .fechaProgramada(citaPartida.getFechaProgramada()
                        .plus(citaPartida.getDuracion(), ChronoUnit.SECONDS))
                .build();
    }
}
