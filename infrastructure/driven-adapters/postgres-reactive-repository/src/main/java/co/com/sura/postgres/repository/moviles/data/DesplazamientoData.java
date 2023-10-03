package co.com.sura.postgres.repository.moviles.data;

import co.com.sura.postgres.repository.agenda.data.CitaData;
import com.fasterxml.jackson.annotation.JsonFormat;
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
@Table("desplazamientos")
public class DesplazamientoData {
    @Id
    @Column("id_desplazamiento")
    private Integer idDesplazamiento;

    @Column("id_cita_partida")
    private String idCitaPartida;

    @Column("id_cita_destino")
    private String idCitaDestino;

    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd HH:mm")
    @Column("fecha_programada")
    private LocalDateTime fechaProgramada;

    @Column("id_horario_turno")
    private Integer idHorarioTurno;

    @Column("id_estado")
    private Integer idEstado;

    private String tipo;
    private Integer duracion;
    private Integer holgura;

    @Column("id_movil")
    private String idMovil;

    @Column("id_profesional")
    private String idProfesional;

    public static DesplazamientoData crearDesplazamientoData (CitaData citaPartida , CitaData citaDestino){
        return DesplazamientoData
                .builder()
                .idCitaPartida(citaPartida.getIdCita())
                .idCitaDestino(citaDestino.getIdCita())
                .fechaProgramada(citaPartida.getFechaProgramada()
                        .plus(citaPartida.getDuracion(), ChronoUnit.SECONDS))
                .build();
    }
}
