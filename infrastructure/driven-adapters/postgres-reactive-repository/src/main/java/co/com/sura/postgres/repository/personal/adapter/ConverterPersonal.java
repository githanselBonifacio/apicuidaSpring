package co.com.sura.postgres.repository.personal.adapter;

import co.com.sura.entity.maestro.HorarioTurno;
import co.com.sura.entity.personal.Conductor;
import co.com.sura.entity.personal.Movil;
import co.com.sura.entity.personal.Profesional;
import co.com.sura.entity.personal.ProfesionalWithTurno;
import co.com.sura.entity.personal.TurnoProfesional;
import co.com.sura.entity.remision.datosremision.ItemSecuenciaTurno;
import co.com.sura.postgres.Converter;
import co.com.sura.postgres.repository.personal.data.ConductorData;
import co.com.sura.postgres.repository.personal.data.ItemSecuenciaTurnoData;
import co.com.sura.postgres.repository.personal.data.MovilData;
import co.com.sura.postgres.repository.personal.data.ProfesionalData;
import co.com.sura.postgres.repository.personal.data.TurnoProfesionalesData;

import java.util.Collections;

public class ConverterPersonal extends Converter {



    public static Profesional convertToProfesional(ProfesionalData profesionalData){
        return converToEntity(profesionalData, Profesional.class);
    }
    public static ProfesionalData convertToProfesionalData(Profesional profesional){
        return converToEntity(profesional, ProfesionalData.class);
    }

    public static ProfesionalWithTurno convertToProfesionalTurno(ProfesionalData profesionalData){
        return converToEntity(profesionalData, ProfesionalWithTurno.class);
    }
    public static TurnoProfesional convertToTurnoProfesional(TurnoProfesionalesData turnoProfesionalesData){
        return converToEntity(turnoProfesionalesData, TurnoProfesional.class);
    }
    public static TurnoProfesionalesData convertToTurnoProfesionalData(TurnoProfesional turnoProfesional){
        return converToEntity(turnoProfesional, TurnoProfesionalesData.class);
    }

    public  static Conductor converToConductor(ConductorData conductorData){
        return converToEntity(conductorData, Conductor.class);
    }
    public  static ConductorData converToConductorData(Conductor conductor){
        return converToEntity(conductor, ConductorData.class);
    }
    public static Movil convertToMovil(MovilData movilData){
        return  converToEntity(movilData, Movil.class);
    }
    public static MovilData convertToMovilData(Movil movil){
        return   converToEntity(movil, MovilData.class);
    }

    public static ItemSecuenciaTurno convertToSecuenciaTurno(ItemSecuenciaTurnoData secuenciaTurnoData){
        return ItemSecuenciaTurno
                .builder()
                .nombreSecuencia(secuenciaTurnoData.getNombreSecuencia())
                .descripcion(secuenciaTurnoData.getDescripcion())
                .numeroDia(secuenciaTurnoData.getNumeroDia())
                .nombreDia(secuenciaTurnoData.getNombreDia())
                .horariosTurno(Collections.singletonList(
                        HorarioTurno.builder().id(secuenciaTurnoData.getIdHorarioTurno()).build()))
                .build();
    }
}
